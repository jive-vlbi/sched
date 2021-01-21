from .catalog import SourceCatalog, StationCatalog, ScanCatalog, \
    SourcePlotCatalog
from .sched import parameter
from . import util
from .plot_util import shut_up_mpl

import schedlib as s

# import matplotlib and call .use immediately
import matplotlib
matplotlib.use("Qt5Agg")
matplotlib.rcParams["toolbar"] = "toolmanager"

# in matplotlib version 3.3 the internal epoch was changed from year 1 to 1970
# this epoch is used to convert dates to floating points internally
# unfortunately that internal representation leaks to axis.get_xlim
# so set the epoch to the old version here to get consistent behaviour with
# both version (<3.3 and >=3.3)
from distutils.version import LooseVersion
if LooseVersion(matplotlib.__version__) >= LooseVersion("3.3"):
    matplotlib.rcParams["date.epoch"] = "0000-12-31T00:00:00"

# import function to improve toolbar, but might not be available on all systems
try:
    from .plot_toolbar import adjust as adjust_toolbar
except ImportError:
    def adjust_toolbar(*args, **kwargs):
        pass

from matplotlib.backends.backend_qt5agg import FigureCanvasQTAgg as FigureCanvas
import matplotlib.figure
import matplotlib.pyplot as plt
import matplotlib.dates
import matplotlib.units
import matplotlib.cbook
from matplotlib.colors import to_hex

# hack around bug in formlayout, set an environment variable to force use of Qt5
import os; os.environ["QT_API"] = "pyqt5"
from formlayout import ColorButton

import numpy as np

from astropy.time import Time

from PyQt5.QtWidgets import QApplication, QWidget, QFrame, QScrollArea, \
    QTabWidget, QPushButton, QCheckBox, QGroupBox, QLabel, QLineEdit, \
    QComboBox, QMenu, QRadioButton, \
    QVBoxLayout, QHBoxLayout, QGridLayout, QButtonGroup, \
    QMessageBox
from PyQt5.QtCore import pyqtSignal, Qt
from PyQt5.QtGui import QCursor, QDoubleValidator, QColor

import itertools
from collections import defaultdict, OrderedDict
import math
from datetime import datetime, timedelta, date
from contextlib import contextmanager

# list of colors to cycle through
color_list = [to_hex(c) for c in "bgrcmyk"] + ["orange", "brown"] 

@contextmanager
def wait_cursor():
    QApplication.setOverrideCursor(QCursor(Qt.WaitCursor))
    yield
    QApplication.restoreOverrideCursor()

# update matplotlib date converter to handle None, 
# as that value is used to signal a line segment end
def _to_ordinalf(d):
    if d is None:
        return None
    if isinstance(d, datetime):
        seconds = (d - datetime(d.year, d.month, d.day, tzinfo=d.tzinfo)).\
            total_seconds()
    else:
        seconds = 0
    return d.toordinal() + (seconds / 24 / 60 / 60)
_to_ordinalf_np_vectorized = np.vectorize(_to_ordinalf)
class NoneDateConverter(type(matplotlib.units.registry[datetime])):
    @staticmethod
    def convert(value, unit, axis):
        if matplotlib.units.ConversionInterface.is_numlike(value):
            return value
        else:
            if not np.iterable(value):
                return _to_ordinalf(value)
            else:
                d = np.asarray(value)
                if not d.size:
                    return d
                return _to_ordinalf_np_vectorized(d)
matplotlib.units.registry[datetime] = NoneDateConverter()
time_formatter = matplotlib.dates.DateFormatter("%H:%M:%S")

def mjd2utc(mjd):
    return Time(mjd, format="mjd").datetime

def mjd2lst(mjd, longitude):
    # the base date used is deterministic but undefined
    # (only) useful for plotting
    _, _, lstday = s.sidtim(mjd, longitude, math.pi * 2)
    return epoch2datetime(lstday)

def epoch2datetime(x):
    fractional, integer = math.modf(x)
    return datetime.fromordinal(int(integer)) + \
        timedelta(seconds=round(fractional * 86400))

class FloatEdit(QLineEdit):
    def __init__(self, initial="0.0", *args, **kwargs):
        super().__init__(initial, *args, **kwargs)
        self.setValidator(QDoubleValidator(self))

    def value(self):
        try:
            return float(self.text())
        except ValueError:
            return None

class MarkerSelection(QComboBox):
    description_character = {
        "point": ".",
        "pixel": ",",
        "circle": "o",
        "triangle_down": "v",
        "triangle_up": "^",
        "triangle_left": "<",
        "triangle_right": ">",
        "tri_down": "1",
        "tri_up": "2",
        "tri_left": "3",
        "tri_right": "4",
        "octagon": "8",
        "square": "s",
        "pentagon": "p",
        "star": "*",
        "hexagon1": "h",
        "hexagon2": "H",
        "plus": "+",
        "x": "x",
        "diamond": "D",
        "thin_diamond": "d",
        "vline": "|",
        "hline": "_",
    }

    def __init__(self, parent=None):
        super().__init__(parent)
        self.addItems(sorted(self.description_character.keys()))

    def get_marker(self):
        return self.description_character[self.currentText()]

class SourcesWidget(QGroupBox):
    def __init__(self, sources, parent=None):
        super().__init__("Sources", parent)
        main_layout = QVBoxLayout(self)
        scroll = QScrollArea(self)
        main_layout.addWidget(scroll)
        sources_widget = QWidget(self)
        sources_layout = QVBoxLayout(sources_widget)
        scroll.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        scroll.setVerticalScrollBarPolicy(Qt.ScrollBarAsNeeded)
        self.source_checkbox = {}
        for source in sources:
            alias = source.aliases[0]
            if len(source.aliases) > 1:
                label = "{} ({})".format(alias, ", ".join(source.aliases[1:]))
            else:
                label = alias
            checkbox = QCheckBox(label, sources_widget)
            checkbox.setChecked(True)
            sources_layout.addWidget(checkbox)
            self.source_checkbox[alias] = checkbox
        all_button = QPushButton("All", self)
        scroll.setWidget(sources_widget)
        main_layout.addWidget(all_button)
        all_button.clicked.connect(self.change_all)

    def selected_sources(self):
        return [source for source, checkbox in self.source_checkbox.items()
                if checkbox.isChecked()]

    def change_all(self):
        to = not any(checkbox.isChecked() 
                     for checkbox in self.source_checkbox.values())
        for checkbox in self.source_checkbox.values():
            checkbox.setChecked(to)

class PlotCheckBox(QWidget):
    def __init__(self, parent=None):
        super().__init__(parent)
        layout = QHBoxLayout(self)
        
        self.check_box = QCheckBox("", self)
        self.clicked = self.check_box.clicked
        layout.addWidget(self.check_box)

        self.figure = matplotlib.figure.Figure()
        canvas = FigureCanvas(self.figure)
        layout.addWidget(canvas)
        axis = self.figure.add_subplot(111)
        axis.xaxis.set_visible(False)
        axis.yaxis.set_visible(False)
        self.line = axis.plot([0,1], [0,1], "r-")[0]
        # force the plot to have about the same size as the check box
        # for some reason the check box size method return a very high 
        # number for width, so use height for both dimensions
        size = self.check_box.size()
        canvas.setFixedSize(size.height(), size.height())
        canvas.draw()

    def isChecked(self):
        return self.check_box.isChecked()

    def setChecked(self, to):
        return self.check_box.setChecked(to)

    def get_color(self):
        return self.line.get_color()

    def set_color(self, color):
        r = self.line.set_color(color)
        self.figure.canvas.draw()
        return r

    def get_visible(self):
        return self.figure.get_visible()

    def set_visible(self, to):
        r = self.figure.set_visible(to)
        self.figure.canvas.draw()
        return r

    def get_linewidth(self):
        return self.line.get_linewidth()

    def set_linewidth(self, width):
        r = self.line.set_linewidth(width)
        self.figure.canvas.draw()
        return r

    def get_properties(self):
        return plt.getp(self.line)

class HighlightGroup(QWidget):
    choices = ("Hide", "Show", "Mark")
    choices_text = {c: c[0] for c in choices}
    text_choices = {c[0]: c for c in choices}
    clicked = pyqtSignal()
    def __init__(self, parent=None):
        super().__init__(parent)
        layout = QHBoxLayout(self)
        self.buttons = QButtonGroup(self)
        for index, choice in enumerate(self.choices):
            button = QRadioButton(self.choices_text[choice])
            button.clicked.connect(self.clicked)
            self.buttons.addButton(button, index)
            layout.addWidget(button)

    def current_text(self):
        return self.text_choices[self.buttons.checkedButton().text()]

    def select(self, text):
        self.buttons.button(self.choices.index(text)).setChecked(True)
        
    def setChecked(self, to):
        if to:
            self.select("Show")
        else:
            self.select("Hide")

class BaselinesWidget(QGroupBox):
    changed = pyqtSignal()
    def _toggle(self, widgets):
        check = not any(w.isChecked() for w in widgets)
        for w in widgets:
            w.setChecked(check)
        self.changed.emit()
    
    def _handle_row(self, station):
        self._toggle([box for (row, _), box in self.check_box.items()
                      if row == station])

    def _handle_column(self, station):
        self._toggle([box for (_, column), box in self.check_box.items()
                      if column == station])

    def _handle_station(self, station):
        self._toggle([box for baseline, box in self.check_box.items()
                      if station in baseline])

    def _handle_all(self):
        self._toggle(list(self.check_box.values()))

    def __init__(self, stations, title, checkbox_type, parent=None):
        super().__init__(title, parent)
        scroll_layout = QVBoxLayout(self)
        self.setLayout(scroll_layout)
        scroll_area = QScrollArea(self)
        scroll_layout.addWidget(scroll_area)
        scroll_area.setHorizontalScrollBarPolicy(Qt.ScrollBarAsNeeded)
        scroll_area.setVerticalScrollBarPolicy(Qt.ScrollBarAsNeeded)
        scroll_widget = QWidget(scroll_area)
        selection_layout = QGridLayout(scroll_widget)
        self.check_box = {} # {(station_row, station_column): checkbox_type()}
        all_button = QPushButton("All", scroll_widget)
        all_button.clicked.connect(self._handle_all)
        selection_layout.addWidget(all_button, 0, 0)

        for column, header in enumerate(stations[1:], 1):
            button = QPushButton("\n".join(header.station), scroll_widget)
            button.clicked.connect(lambda _, s=header.station: 
                                   self._handle_station(s))
            selection_layout.addWidget(button, 0, column)

        for row, header in enumerate(stations[:-1], 1):
            button = QPushButton(header.station, scroll_widget)
            button.clicked.connect(lambda _, s=header.station: 
                                   self._handle_station(s))
            selection_layout.addWidget(button, row, 0)
            for column, item in enumerate(stations[row:], row):
                check_box = checkbox_type(parent=scroll_widget)
                check_box.setChecked(True)
                check_box.clicked.connect(self.changed)
                self.check_box[(header.station, item.station)] = check_box
                selection_layout.addWidget(check_box, row, column)
        scroll_area.setWidget(scroll_widget)

class BaselineSelectionWidget(BaselinesWidget):
    def get_visible(self, baseline):
        return self.check_box[baseline].isChecked()

class BaselineHighlightWidget(BaselinesWidget):
    def __init__(self, stations, parent=None):
        super().__init__(stations, 
                         "Select H(ide), S(how) or M(ark) per baseline", 
                         HighlightGroup, 
                         parent)

    def _toggle(self, widgets):
        min_index = min(HighlightGroup.choices.index(w.current_text()) 
                        for w in widgets)
        to = HighlightGroup.choices[
            (min_index + 1) % len(HighlightGroup.choices)]
        for w in widgets:
            w.select(to)

    def get_properties(self, baseline):
        highlight = self.check_box[baseline].current_text() == "Mark"
        return {"linewidth": 3 if highlight else 1,
                "color": "b" if highlight else "r"}

    def get_visible(self, baseline):
        return self.check_box[baseline].current_text() != "Hide"


class BaselineConfigurationWidget(QWidget):
    def _set_plot(self):
        to = self.plot_check_box.isChecked()
        for w in self.baselines.check_box.values():
            if w.isChecked():
                w.set_visible(to)

    def _set_color(self):
        color = self.color_button.get_color().name()
        for w in self.baselines.check_box.values():
            if w.isChecked():
                w.set_color(color)

    def _set_width(self):
        width = self.width_edit.value()
        if width is not None:
            for w in self.baselines.check_box.values():
                if w.isChecked():
                    w.set_linewidth(width)

    def check_update(self):
        if self.automatic_update.isChecked():
            attributes = {"color": (lambda c: self.color_button.set_color(
                              QColor(c))), 
                          "visible": self.plot_check_box.setChecked, 
                          "linewidth": (lambda v: self.width_edit.setText(
                              str(v)))}
            value_count = {attribute: defaultdict(int) 
                           for attribute in attributes.keys()}
            for w in self.baselines.check_box.values():
                if w.isChecked():
                    for attribute in attributes.keys():
                        value = getattr(w, "get_" + attribute)()
                        value_count[attribute][value] += 1
            if len(value_count["color"]) == 0:
                return # nothing selected, do nothing
            for attribute, function in attributes.items():
                value = max((count, value) for value, count in 
                            value_count[attribute].items())[1]
                function(value)

    def get_properties(self, baseline):
        check_box = self.baselines.check_box[baseline]
        return {"linewidth": check_box.get_linewidth(),
                "color": check_box.get_color()}

    def get_visible(self, baseline):
        return self.baselines.check_box[baseline].get_visible()

    def __init__(self, stations, parent=None):
        super().__init__(parent)
        master_layout = QVBoxLayout(self)
        
        self.baselines = BaselinesWidget(
            stations, "Select baselines to configure", PlotCheckBox, self)
        self.baselines.changed.connect(self.check_update)
        master_layout.addWidget(self.baselines)

        group_box = QGroupBox("Configure selection", self)
        control_layout = QHBoxLayout(group_box)
        control_layout.addStretch(1)
        def add_separator():
            control_layout.addStretch(1)
            line = QFrame(group_box)
            line.setFrameShape(QFrame.VLine)
            line.setFrameShadow(QFrame.Sunken)
            line.setFixedWidth(3)
            control_layout.addWidget(line)
            control_layout.addStretch(1)

        self.automatic_update = QCheckBox("Update", group_box)
        self.automatic_update.setToolTip(
            "If checked, configuration values are updated when selection "
            "changes, to the most common values")
        self.automatic_update.setChecked(True)
        control_layout.addWidget(self.automatic_update)
        add_separator()
        
        self.plot_check_box = QCheckBox("Plot", group_box)
        control_layout.addWidget(self.plot_check_box)
        button = QPushButton("Set", group_box)
        button.clicked.connect(self._set_plot)
        control_layout.addWidget(button)
        add_separator()
        
        control_layout.addWidget(QLabel("Color", group_box))
        self.color_button = ColorButton(group_box)
        control_layout.addWidget(self.color_button)
        button = QPushButton("Set", group_box)
        button.clicked.connect(self._set_color)
        control_layout.addWidget(button)
        add_separator()

        control_layout.addWidget(QLabel("Width", group_box))
        self.width_edit = FloatEdit("1.0", group_box)
        control_layout.addWidget(self.width_edit)
        button = QPushButton("Set", group_box)
        button.clicked.connect(self._set_width)
        control_layout.addWidget(button)
        
        control_layout.addStretch(1)
        master_layout.addWidget(group_box)

        self.check_update()

class StationsWidget(QGroupBox):
    def get_visible(self, station):
        return self.visible_checkbox[station].isChecked()

    def get_color(self, station):
        return self.color_button[station].get_color().name()

    def get_linewidth(self, station):
        return self.linewidth_edit[station].value()

    def get_properties(self, station):
        ret = {"color": self.get_color(station)}
        lw = self.get_linewidth(station)
        if lw is not None:
            ret["linewidth"] = lw
        return ret

    def __init__(self, stations, parent=None):
        super().__init__("Stations", parent)
        layout = QGridLayout(self)
        self.visible_checkbox = {}
        self.color_button = {}
        self.linewidth_edit = {}
        for column, label in enumerate(("Plot", "Color", "Linewidth"), 1):
            layout.addWidget(QLabel(label, self), 0, column)
        color_cycle = itertools.cycle(color_list)
        for row, station in enumerate(stations, 1):
            name = station.station
            layout.addWidget(QLabel(name, self), row, 0)
            
            checkbox = QCheckBox(self)
            checkbox.setChecked(True)
            self.visible_checkbox[name] = checkbox
            layout.addWidget(checkbox, row, 1)

            color = ColorButton(self)
            color.set_color(QColor(next(color_cycle)))
            self.color_button[name] = color
            layout.addWidget(color, row, 2)

            width = FloatEdit("1.0", self)
            self.linewidth_edit[name] = width
            layout.addWidget(width, row, 3)
            
class UVWidget(QWidget):
    def _check_axis(self):
        self.axis_unit_multiplier.setVisible(
            self.axis_unit.currentText() == "Wv")
            
    wavelength_multiplier = OrderedDict((("λ", 1),
                                         ("Kλ", 1e-3),
                                         ("Mλ", 1e-6)))

    def get_unit_multiplier(self):
        if self.axis_unit.currentText() == "Wv":
            unit = self.axis_unit_multiplier.currentText()
            return (unit, self.wavelength_multiplier[unit])
        else:
            return ("Km", 1)

    def __init__(self, sources, stations, parent=None):
        super().__init__(parent)
        master_layout = QVBoxLayout(self)
        
        top_layout = QHBoxLayout()
        top_layout.addWidget(QLabel("Axis units", self))
        self.axis_unit = QComboBox(self)
        self.axis_unit.addItems(["Km", "Wv"])
        self.axis_unit.currentTextChanged.connect(self._check_axis)
        top_layout.addWidget(self.axis_unit)
        self.axis_unit_multiplier = QComboBox(self)
        self.axis_unit_multiplier.addItems(self.wavelength_multiplier.keys())
        top_layout.addWidget(self.axis_unit_multiplier)
        top_layout.addStretch(1)
        self._check_axis()
        self.sources = SourcesWidget(sources, self)
        top_layout.addWidget(self.sources)
        top_layout.setStretchFactor(self.sources, 1)
        master_layout.addLayout(top_layout)

        self.tab_widget = QTabWidget(self)
        self.tab_widget.addTab(
            BaselineHighlightWidget(stations, self.tab_widget), "Highlight")
        self.tab_widget.addTab(
            BaselineConfigurationWidget(stations, self.tab_widget), "Advanced")
        master_layout.addWidget(self.tab_widget)

    def get_configuration_widget(self):
        return self.tab_widget.currentWidget()

class XYBaseWidget(QWidget): # shared by XY and Uptime
    def _check_x_axis(self):
        x = self.x_axis.currentText()
        self.ut_offset.setVisible(x == "UT")
        self.lst_base.setVisible(x == "LST")

    def get_x_axis(self):
        return self.x_axis.currentText()

    def get_ut_offset(self):
        return int(self.ut_offset.currentText())

    def get_lst_base(self):
        return self.lst_base.currentText()

    def get_y_axis(self):
        if len(self.y_items) > 1:
            return self.y_axis.currentText()
        else:
            return self.y_items[0]
    
    def __init__(self, x_items, y_items, sources, stations, parent=None):
        super().__init__(parent)
        self.y_items = y_items
        master_layout = QHBoxLayout(self)
        
        left_layout = QVBoxLayout()
        left_layout.addStretch(1)

        top_left_layout = QHBoxLayout()
        left_layout.addStretch(1)
        top_left_layout.addWidget(QLabel("X", self))
        self.x_axis = QComboBox(self)
        self.x_axis.addItems(x_items)
        self.x_axis.currentTextChanged.connect(self._check_x_axis)
        top_left_layout.addWidget(self.x_axis)
        self.ut_offset = QComboBox(self)
        self.ut_offset.addItems(str(i) for i in range(-11, 13))
        self.ut_offset.setCurrentText("0")
        top_left_layout.addWidget(self.ut_offset)
        self.lst_base = QComboBox(self)
        self.lst_base.addItems(["Greenwich"] + [e.station for e in stations])
        top_left_layout.addWidget(self.lst_base)
        top_left_layout.addStretch(1)
        if len(self.y_items) > 1:
            top_left_layout.addWidget(QLabel("Y", self))
            self.y_axis = QComboBox(self)
            self.y_axis.addItems(y_items)
            top_left_layout.addWidget(self.y_axis)

        self._check_x_axis()
        left_layout.addLayout(top_left_layout)
        
        left_layout.addStretch(1)
        
        self.stations = StationsWidget(stations, self)
        left_layout.addWidget(self.stations)
        left_layout.addStretch(1)

        master_layout.addLayout(left_layout)

        self.sources = SourcesWidget(sources, self)
        master_layout.addWidget(self.sources)

class XYWidget(XYBaseWidget):
    def __init__(self, sources, stations, parent=None):
        y_items = ["AZ", "EL",  "HA", "PA", "Sec"]
        x_items = ["UT", "GST", "LST"] + y_items
        super().__init__(x_items, y_items, sources, stations, parent)

class UptimeWidget(XYBaseWidget):
    def __init__(self, sources, stations, parent=None):
        x_items = ["UT", "GST", "LST"]
        y_items = ["Antenna"]
        super().__init__(x_items, y_items, sources, stations, parent)

class RADecWidget(QWidget):
    def get_visible(self, calcode):
        return self.widgets[calcode]["Plot"].isChecked()

    def get_properties(self, calcode):
        widgets = self.widgets[calcode]
        ret = {"marker": widgets["Marker"].get_marker(),
               "color": widgets["Color"].get_color().name()}
        lw = widgets["Linewidth"].value()
        if lw is not None:
            ret["linewidth"] = lw
        s = widgets["Size"].value()
        if s is not None:
            ret["s"] = s
        return ret

    calcode_map = {
        "Y": "VLA",
        "N": "USNO",
        "M": "MERLIN",
        "V": "VLBA"
    }
    marker_map = {
        "Y": "square",
        "N": "triangle_right",
        "M": "thin_diamond",
        "V": "star"
    }
    def __init__(self, sources, parent=None):
        super().__init__(parent)
        calcodes = set(s.srlcalc for s in sources)
        
        layout = QGridLayout(self)

        columns = OrderedDict([
            ("Plot", QCheckBox),
            ("Marker", MarkerSelection),
            ("Color", ColorButton), 
            ("Size", lambda parent: FloatEdit("100", parent)), 
            ("Linewidth", lambda parent: FloatEdit("3", parent))])

        self.widgets = {}

        layout.addWidget(QLabel("Catalog", self), 0, 0, Qt.AlignBottom)
        for column, label in enumerate(columns.keys(), 1):
            layout.addWidget(QLabel(label, self), 0, column, Qt.AlignBottom)

        color_cycle = itertools.cycle(color_list)
        marker_cycle = itertools.cycle(sorted(
            set(MarkerSelection.description_character.keys()) -
            set(self.marker_map.values())))
        for row, calcode in enumerate(sorted(calcodes) + ["SCHED"], 1):
            catalog = self.calcode_map.get(calcode, calcode)
            layout.addWidget(QLabel(catalog, self), row, 0)
            widgets = {}
            for column, (column_label, widget_function) in \
                enumerate(columns.items(), 1):
                w = widget_function(self)
                widgets[column_label] = w
                layout.addWidget(w, row, column)

            self.widgets[calcode] = widgets
            # initialize widgets
            widgets["Plot"].setChecked(True)
            if calcode in self.marker_map:
                marker = self.marker_map[calcode]
            else:
                marker = next(marker_cycle)
            widgets["Marker"].setCurrentText(marker)
            widgets["Color"].set_color(QColor(next(color_cycle)))

class BeamWidget(QWidget):
    def get_source(self):
        return self.source.currentText()

    def get_setup_index(self):
        if len(self.wave_index) == 0:
            return None
        return self.wave_index[self.wavelength.currentIndex()][1]

    def get_oversampling(self):
        return int(self.oversampling.currentText())

    def get_weighting(self):
        return self.weight.currentText()

    def get_image_transfer_function(self):
        return self.image_transfer_function.currentText()

    def get_color_map(self):
        return matplotlib.cm.get_cmap(self.color_map.currentText())

    def get_contour_colors(self):
        return matplotlib.cm.get_cmap(self.contour_colors.currentText())

    def get_contours(self):
        return self.contours.isChecked()

    def __init__(self, sources, stations, scans, parent=None):
        super().__init__(parent)
        master_layout = QVBoxLayout(self)
        top_layout = QGridLayout()
        
        used_setups = set(scan.setnum for scan in scans)
        self.wave_index = sorted((300 / s.schsf.sffreq[0, i-1], i)
                                 for i in used_setups
                                 if s.schsf.sffreq[0, i-1] > 0)
        for row, (label, attribute, options) in enumerate((
                ("Source", "source", [s.aliases[0] for s in sources]),
                ("Wavelength (cm)", "wavelength", 
                 ["{:.1f}".format(w[0] * 100).rstrip("0").rstrip(".") + 
                  " ({})".format(util.f2str(s.schssf.setfile[w[1]]))
                  for w in self.wave_index]),
                ("Oversampling factor", "oversampling", map(str, range(1, 11))),
                ("Weight", "weight", ["Natural", "Uniform"]),
                ("Image transfer function", "image_transfer_function", 
                 ["Linear", "Logarithmic", "Square-root"]),
                ("Color palette", "color_map", 
                 sorted(plt.colormaps(), key=lambda x: x.lower()))
        )):
            top_layout.addWidget(QLabel(label, self), row, 0)
            w = QComboBox(self)
            setattr(self, attribute, w)
            w.addItems(options)
            top_layout.addWidget(w, row, 1)
        self.color_map.setCurrentText("Greys")
        
        self.contours = QCheckBox("Plot contours", self)
        top_layout.addWidget(self.contours, row + 1, 0)
        self.contour_colors = QComboBox(self)
        self.contour_colors.addItems(sorted(plt.colormaps(), 
                                            key=lambda x: x.lower()))
        self.contour_colors.setCurrentText("Spectral")
        top_layout.addWidget(self.contour_colors, row + 1, 1)

        master_layout.addLayout(top_layout)
        
        self.baselines = BaselineSelectionWidget(
            stations, "Baselines to include", QCheckBox, self)
        master_layout.addWidget(self.baselines)

class MainWidget(QWidget):
    finished = pyqtSignal()
    
    def show_plot(self):
        active = self.tab_widget.tabText(self.tab_widget.currentIndex())
        {"UV": self.plot_uv,
         "XY": self.plot_xy,
         "Uptime": self.plot_uptime,
         "RA-Dec": self.plot_radec,
         "Beam": self.plot_beam}[active]()

    def __init__(self, is_restart, parent=None):
        super().__init__(parent)
        window_layout = QVBoxLayout(self)
        self.setLayout(window_layout)
        
        source_catalog = SourceCatalog()
        source_catalog.read()
        source_catalog.set_aliases()
        self.sources = source_catalog.used()
        self.radec_sources = [e for e in SourcePlotCatalog().read() 
                              if not e.srlused]
        self.scan_catalog = ScanCatalog()
        self.scan_catalog.read()
        station_catalog = StationCatalog()
        station_catalog.read()
        station_catalog.read_scheduled_attributes()
        self.stations = station_catalog.used()
        
        self.tab_widget = QTabWidget(self)
        self.uv = UVWidget(self.sources, self.stations, self.tab_widget)
        self.tab_widget.addTab(self.uv, "UV")
        self.xy = XYWidget(self.sources, self.stations, self.tab_widget)
        self.tab_widget.addTab(self.xy, "XY")
        self.uptime = UptimeWidget(self.sources, self.stations, self.tab_widget)
        self.tab_widget.addTab(self.uptime, "Uptime")
        self.radec = RADecWidget(self.radec_sources, self.tab_widget)
        self.tab_widget.addTab(self.radec, "RA-Dec")
        self.beam = BeamWidget(self.sources, self.stations, 
                               self.scan_catalog.used(), self.tab_widget)
        self.tab_widget.addTab(self.beam, "Beam")
        
        window_layout.addWidget(self.tab_widget)

        control_widget = QWidget(self)
        control_layout = QHBoxLayout(control_widget)
        control_widget.setLayout(control_layout)
        plot_button = QPushButton("Plot", control_widget)
        control_layout.addWidget(plot_button)
        plot_menu = QMenu("Plot", plot_button)
        plot_button.clicked.connect(self.show_plot)
        restart_button = QPushButton("Restart", control_widget)
        control_layout.addWidget(restart_button)
        if not is_restart:
            finish_button = QPushButton("Finish", control_widget)
            control_layout.addWidget(finish_button)
            finish_button.clicked.connect(lambda: self.done(1))
        exit_button = QPushButton("Exit", control_widget)
        control_layout.addWidget(exit_button)
        exit_button.clicked.connect(lambda: self.done(0))
        restart_button.clicked.connect(lambda: self.done(2))
        window_layout.addWidget(control_widget)

        self._result = None

    # replicate signal functionality of a QDialog, without the window behaviour
    def done(self, r):
        self._result = r
        self.finished.emit()

    def result(self):
        # 0 -> exit, 1 -> finish, 2 -> restart
        # just exit if no result has been set (window closed manually)
        return self._result if self._result is not None else 0

    def plot_uv(self):
        with wait_cursor():
            plot_sources = self.uv.sources.selected_sources()
            if len(plot_sources) == 0:
                return
            alias_source = {}
            for source in self.sources:
                if source.aliases[0] in plot_sources:
                    for alias in source.aliases:
                        alias_source[alias] = source.aliases[0]
            
            configuration = self.uv.get_configuration_widget()
            # plot sources in a square pattern, expanding columns before rows
            items = len(plot_sources)
            columns = math.ceil(math.sqrt(items))
            rows = math.ceil(items / columns)
            with shut_up_mpl():
                figure, axes = plt.subplots(rows, columns, squeeze=False)
            figure.canvas.set_window_title("UV Plot")
            axes = axes.reshape((rows * columns,))

            unit, multiplier = self.uv.get_unit_multiplier()
            scan_multiplier = multiplier
            
            # Get parameters for plotting of multiple lines for MFS.
            # MFSRAT is the ratio of the upper to the lower frequency.
            # NMFS is the number of frequencies to use.
            if s.schcon.nmfs > 1:
                # multi frequency synthesis plots requested
                mfs_base = 2 / (1 + s.schcon.mfsrat)
                mfs_inc = mfs_base * (s.schcon.mfsrat - 1) / (s.schcon.nmfs - 1)
            else:
                mfs_base = 1
                mfs_inc = 0
            
            source_baseline_xy = defaultdict(
                lambda: defaultdict(lambda: [[], []]))
            for scan_index, scan in enumerate(self.scan_catalog.used(), 
                                              self.scan_catalog.scan_offset):
                if scan.scnsrc not in alias_source:
                    continue
                source = alias_source[scan.scnsrc]
                
                if unit.endswith("λ"):
                    frequency = s.schsf.sffreq[0, scan.setnum-1]
                    # frequency is in MHz
                    wavelength = 3e8 / (frequency * 1e6)
                    # data is in Km
                    scan_multiplier = multiplier / wavelength * 1000

                station_uv = {station: np.array(s.stauv(scan_index+1, i+1)).\
                              reshape((2, 2)) * scan_multiplier
                              for i, station in enumerate(self.stations)}
                for mfs in (mfs_base + i * mfs_inc 
                            for i in range(s.schcon.nmfs)):
                    mfs_station_uv = {k: v * mfs for k, v in station_uv.items()}
                
                    baselines = itertools.combinations(self.stations, 2)
                    for station1, station2 in baselines:
                        station_names = (station1.station, station2.station)
                        if configuration.get_visible(station_names) and \
                           station1.stascn[scan_index] and \
                           station2.stascn[scan_index]:
                            diff = mfs_station_uv[station1] - \
                                   mfs_station_uv[station2]
                            xy = source_baseline_xy[source][station_names]
                            for i in range(2): # loop over x (u) and y (v)
                                l = xy[i]
                                l.extend(diff[i])
                                l.append(None)
                                l.extend(-diff[i])
                                l.append(None)
            for source, baseline_xy in source_baseline_xy.items():
                source_index = plot_sources.index(source)
                axis = axes[source_index]
                axis.set_aspect("equal")
                axis.set_title(source)
                axis.xaxis.set_label_text("U ({})".format(unit))
                axis.yaxis.set_label_text("V ({})".format(unit))
                for baseline, [x, y] in baseline_xy.items():
                        axis.plot(
                            x, y, label="{} - {}".format(*baseline),
                            **configuration.get_properties(baseline))
            figure.subplots_adjust(left=0.05, right=0.95,
                                   bottom=0.05, top=0.95,
                                   wspace=.5, hspace=.5)
            # hide unused axes
            for axis in axes[len(plot_sources):]:
                axis.set_visible(False)

            adjust_toolbar(figure)
            figure.tight_layout()

    def plot_xy(self):
        with wait_cursor():
            plot_sources = self.xy.sources.selected_sources()
            if len(plot_sources) == 0:
                return
            alias_source = set()
            for source in self.sources:
                if source.aliases[0] in plot_sources:
                    alias_source.update(source.aliases)

            with shut_up_mpl():
                figure, axis = plt.subplots()
            station_xy = defaultdict(lambda: [[], []])

            axis_type = [self.xy.get_x_axis(), 
                         self.xy.get_y_axis()]

            figure.canvas.set_window_title("{}-{} Plot".format(
                *axis_type[::-1]))

            axis_function = []
            for type_ in axis_type:
                if type_ == "UT":
                    td = timedelta(hours=self.xy.get_ut_offset())
                    def f(scan_index, station, td=td):
                        scan = self.scan_catalog.entries[scan_index]
                        return [mjd2utc(scan.startj) + td, 
                                mjd2utc(scan.stopj) + td,
                                None]
                elif type_ in ["GST", "LST"]:
                    if type_ == "LST":
                        lst_base = self.xy.get_lst_base()
                        if lst_base == "Greenwich":
                            longitude = 0
                        else:
                            longitude = next(station.long_bn 
                                             for station in self.stations
                                             if station.station == lst_base)
                    else:
                        longitude = 0
                    def f(scan_index, station, longitude=longitude): 
                        start = mjd2lst(scan.startj, longitude)
                        stop = mjd2lst(scan.stopj, longitude)
                        return [start, stop, None]
                elif type_ == "AZ":
                    def f(scan_index, station):
                        return [station.az1[scan_index] % 360,
                                station.az2[scan_index] % 360,
                                None]
                elif type_ in ["EL", "HA", "PA"]:
                    start_attribute = "{}{}".format(type_.lower(), 1)
                    end_attribute = "{}{}".format(type_.lower(), 2)
                    def f(scan_index, station, 
                          sa=start_attribute, ea=end_attribute):
                        return [getattr(station, sa)[scan_index],
                                getattr(station, ea)[scan_index],
                                None]
                elif type_ == "Sec":
                    def f(scan_index, station):
                        start = 1 / math.sin(
                            station.el1[scan_index] * parameter.raddeg)
                        end = 1 / math.sin(
                            station.el2[scan_index] * parameter.raddeg)
                        return [start, end, None]
                else:
                    raise RuntimeError("Unhandled axis type {}".format(type_))
                axis_function.append(f)


            for scan_index, scan in enumerate(self.scan_catalog.used(), 
                                              self.scan_catalog.scan_offset):
                if scan.scnsrc not in alias_source:
                    continue
                for station in self.stations:
                    if not (self.xy.stations.get_visible(station.station) and
                            station.stascn[scan_index]):
                        continue

                    xy = station_xy[station]
                    for axis_index, function in enumerate(axis_function):
                        xy[axis_index].extend(function(scan_index, station))
            
            axis.set_title("{} vs {}".format(*axis_type[::-1]))
            axis.xaxis.set_label_text(axis_type[0])
            axis.yaxis.set_label_text(axis_type[1])
            for station, [x, y] in station_xy.items():
                name = station.station
                axis.plot(x, y, label=name,
                          **self.xy.stations.get_properties(name))

            if axis_type[0] in ["UT", "LST", "GST"]:
                axis.xaxis.set_major_formatter(time_formatter)
                figure.autofmt_xdate()
                if axis_type[0] == "UT":
                    # Cludge to update the label of a time axis to reflect the
                    # day of the displayed times.
                    # Updating the axis label within the draw event doesn't 
                    # update the draw title, so have to call draw again. 
                    # Use a boolean to prevent infinite recursion.
                    call_draw = True
                    def draw_event(event):
                        nonlocal call_draw
                        if call_draw:
                            start = epoch2datetime(axis.get_xlim()[0])
                            axis.xaxis.set_label_text(
                                f"Starting at UT {str(start)}")
                            call_draw = False
                            figure.canvas.draw()
                        else:
                            call_draw = True
                    figure.canvas.mpl_connect("draw_event", draw_event)
                else:
                    axis.xaxis.lst_base_date = date.fromordinal(
                        int(axis.get_xlim()[0]))
            if axis_type[0] == "Sec":
                axis.invert_xaxis()
            if axis_type[1] == "Sec":
                axis.invert_yaxis()
            
            legend = axis.legend()
            legend.set_draggable(True)
            adjust_toolbar(figure, axis_type[0])
            figure.tight_layout()

    def plot_uptime(self):
        with wait_cursor():
            plot_sources = self.uptime.sources.selected_sources()
            if len(plot_sources) == 0:
                return
            alias_source = {}
            for source in self.sources:
                if source.aliases[0] in plot_sources:
                    for alias in source.aliases:
                        alias_source[alias] = source.aliases[0]

            stations = [station for station in self.stations
                        if self.uptime.stations.get_visible(station.station)]
            
            with shut_up_mpl():
                figure, axes = plt.subplots(len(plot_sources), squeeze=False,
                                            sharex=True, sharey=True)
            axes = axes.reshape((len(plot_sources),))

            figure.canvas.set_window_title("Uptime Plot")

            axis_function = []
            axis_type = self.uptime.get_x_axis()
            if axis_type == "UT":
                td = timedelta(hours=self.uptime.get_ut_offset())
                def time_function(mjd, td=td):
                    return mjd2utc(mjd) + td 
            elif axis_type in ["GST", "LST"]:
                if axis_type == "LST":
                    lst_base = self.uptime.get_lst_base()
                    if lst_base == "Greenwich":
                        longitude = 0
                    else:
                        longitude = next(station.long_bn 
                                         for station in self.stations
                                         if station.station == lst_base)
                else:
                    longitude = 0
                def time_function(mjd, longitude=longitude): 
                    return mjd2lst(mjd, longitude)
            else:
                raise RuntimeError("Unhandled axis type {}".format(axis_type))

            source_station_left = defaultdict(lambda: defaultdict(list))
            source_station_width = defaultdict(lambda: defaultdict(list))
            scans = self.scan_catalog.used()
            for scan_index, scan in enumerate(scans, 
                                              self.scan_catalog.scan_offset):
                if scan.scnsrc not in alias_source:
                    continue
                
                left = time_function(scan.startj)
                width = scan.stopj - scan.startj

                source = alias_source[scan.scnsrc]
                station_left = source_station_left[source]
                station_width = source_station_width[source]
                for station in stations:
                    if station.stascn[scan_index] and \
                       (station.up1[scan_index] == "") and \
                       (station.up2[scan_index] == ""):
                        station_left[station].append(left)
                        station_width[station].append(width)

            legend_line = {}
            for source, station_left in source_station_left.items():
                axis = axes[plot_sources.index(source)]
                for station_index, station in enumerate(stations):
                    if station in station_left:
                        left = station_left[station]
                        bottom = [len(stations) - station_index] * len(left)
                        legend_line[station.station] = axis.barh(
                            y=bottom, left=left,
                            width=source_station_width[source][station],
                            label=station.station,
                            **self.uptime.stations.get_properties(
                                station.station))
            
            start = time_function(scans[0].startj)
            end = time_function(scans[-1].stopj)
            for axis, source in zip(axes, plot_sources):
                axis.xaxis.set_major_formatter(time_formatter)
                axis.xaxis.set_major_locator(matplotlib.dates.AutoDateLocator())
                axis.set_xbound(lower=start, upper=end)
                axis.yaxis.set_label_text(source)
                axis.tick_params(axis="y", which="both", 
                                 left=False, labelleft=False)
            figure.autofmt_xdate()
            if axis_type == "UT":
                # Cludge to update the label of a time axis to reflect the
                # day of the displayed times.
                # Updating the axis label within the draw event doesn't 
                # update the draw title, so have to call draw again. 
                # Use a boolean to prevent infinite recursion.
                call_draw = True
                def draw_event(event):
                    nonlocal call_draw
                    if call_draw:
                        start = epoch2datetime(axis.get_xlim()[0])
                        axis.xaxis.set_label_text(
                            f"Starting at UT {str(start)}")
                        call_draw = False
                        figure.canvas.draw()
                    else:
                        call_draw = True
                figure.canvas.mpl_connect("draw_event", draw_event)
            else:
                axis.xaxis.set_label_text(axis_type)
                for axis in axes:
                    axis.xaxis.lst_base_date = date.fromordinal(
                        int(axis.get_xlim()[0]))
            
            plotted_stations = [station.station for station in stations
                                if station.station in legend_line]
            station_lines = [legend_line[station] 
                             for station in plotted_stations]
            legend = axes[0].legend(station_lines, plotted_stations, ncol=4,
                                    loc="lower left", bbox_to_anchor=(-0.05, 1))
            legend.set_draggable(True)
            adjust_toolbar(figure, axis_type)
            # force a draw event to have tight layout work with
            # how the figure is formatted when displayed
            figure.canvas.draw()
            figure.tight_layout()

    def plot_radec(self):
        with wait_cursor():
            with shut_up_mpl():
                figure, axis = plt.subplots()
            figure.canvas.set_window_title("RA-Dec Plot")
            label_points = {}
            label_annotations = {}
            # catalogs
            calcodes = set(s.srlcalc for s in self.radec_sources)
            for calcode in sorted(calcodes):
                catalog_label = self.radec.calcode_map.get(calcode, calcode)
                if catalog_label == "":
                    catalog_label = "No label"
                catalog_sources = [s for s in self.radec_sources
                                   if s.srlcalc == calcode]
                xy = np.array(
                    [[s.srlra, s.srldec] for s in catalog_sources])
                xy[:, 0] /= parameter.radhr
                xy[:, 1] /= parameter.raddeg
                points = axis.scatter(xy[:, 0], xy[:, 1], label=catalog_label,
                                      **self.radec.get_properties(calcode))
                points.set_visible(self.radec.get_visible(calcode))
                label_points[catalog_label] = points
                
                # plot source names if they dont clutter (and slow down)
                # the plot
                visible = (len(catalog_sources) < 20) and points.get_visible()
                annotations = []
                for i, s in enumerate(catalog_sources):
                    a = axis.annotate(s.srlname, xy=xy[i])
                    a.set_visible(visible)
                    annotations.append(a)
                label_annotations[catalog_label] = annotations


            # sched
            if self.radec.get_visible("SCHED"):
                xy = np.array([[s.ra2000, s.d2000] for s in self.sources])
                xy[:, 0] /= parameter.radhr
                xy[:, 1] /= parameter.raddeg
                label_points["SCHED"] = axis.scatter(
                    xy[:, 0], xy[:, 1], label="SCHED",
                    **self.radec.get_properties("SCHED"))
                annotations = []
                for i, s in enumerate(self.sources):
                    annotations.append(
                        axis.annotate(s.aliases[0], xy=xy[i]))
                    label_annotations["SCHED"] = annotations

            axis.set_title("Dec - RA\n"
                           "Click in the legend to show/hide points/labels")
            axis.xaxis.set_label_text("RA (Hours)")
            axis.yaxis.set_label_text("Dec (Deg)")
            axis.set_aspect(parameter.raddeg / parameter.radhr)
            axis.set_xbound(lower=0, upper=24)
            axis.invert_xaxis()
            axis.set_ybound(lower=-90, upper=90)
            legend = axis.legend()
            visible_alpha = {True: 1, False: 0.2}
            for artist in legend.legendHandles:
                artist.set_picker(5)
                plot_artist = label_points[artist.get_label()]
                artist.set_alpha(visible_alpha[plot_artist.get_visible()])
            for text in legend.get_texts():
                text.set_picker(5)
                annotations = label_annotations[text.get_text()]
                visible = annotations and annotations[0].get_visible()
                text.set_alpha(visible_alpha[visible])
            legend.set_draggable(True)
            # allow clicking in the legend to show/hide points/labels
            def onpick(event):
                artist_type = type(event.artist)
                if artist_type is matplotlib.collections.PathCollection:
                    # scatter plot type
                    plot_artist = label_points[event.artist.get_label()]
                    visible = not plot_artist.get_visible()
                    plot_artist.set_visible(visible)
                    event.artist.set_alpha(visible_alpha[visible])
                    figure.canvas.draw()
                elif artist_type is matplotlib.text.Text:
                    annotations = label_annotations[event.artist.get_text()]
                    visible = annotations and not annotations[0].get_visible()
                    event.artist.set_alpha(visible_alpha[visible])
                    for annotation in annotations:
                        annotation.set_visible(visible)
            figure.canvas.mpl_connect('pick_event', onpick)
            adjust_toolbar(figure)
            figure.tight_layout()

    def plot_beam(self):
        with wait_cursor():
            setup_number = self.beam.get_setup_index()
            if setup_number is None:
                QMessageBox.warning(self, "Cannot plot beam.",
                                    "Incomplete frequency setup, "
                                    "unable to create beam plot.")
                return
            with shut_up_mpl():
                figure, axis = plt.subplots()
            wavelength = 300 / s.schsf.sffreq[0, setup_number-1]
            wave_text = "{} cm".format(
                "{:.1f}".format(wavelength * 100).rstrip("0").rstrip("."))
            source = self.beam.get_source()
            figure.canvas.set_window_title("Beam Plot ({}, {})".format(
                source, wave_text))

            aliases = set(next(s.aliases for s in self.sources 
                               if s.aliases[0] == source))
            
            def up_and_in(station, scan_index):
                return station.stascn[scan_index] and \
                    (station.up1[scan_index] == "") and \
                    (station.up2[scan_index] == "")

            # schedlib.plbeam expects 2 arrays for u and corresponding v points
            sched_uv_size = 600000 # defined in src/Plot/beam.inc
            u = np.zeros((sched_uv_size, 1))
            v = np.zeros((sched_uv_size, 1))
            uv_index = 0
            for scan_index, scan in enumerate(self.scan_catalog.used(), 
                                              self.scan_catalog.scan_offset):
                if not ((scan.scnsrc in aliases) and 
                        (scan.setnum == setup_number)):
                    continue
                station_uv = {station: np.array(s.stauv(scan_index+1, i+1)).\
                              reshape((2, 2)) / wavelength
                              for i, station in enumerate(self.stations)}
                baselines = itertools.combinations(self.stations, 2)
                for station1, station2 in baselines:
                    station_names = (station1.station, station2.station)
                    if self.beam.baselines.get_visible(station_names) and \
                       up_and_in(station1, scan_index) and \
                       up_and_in(station2, scan_index):
                        diff = station_uv[station1] - station_uv[station2]
                        u[uv_index:uv_index+2, 0] = diff[0, :]
                        v[uv_index:uv_index+2, 0] = diff[1, :]
                        uv_index += 2

            # multiply by factor, because of SCHED
            max_uv_dist = math.sqrt(np.max(u*u + v*v, axis=None)) * 10000 
            weighting_map = {"Natural": 0, "Uniform": 1}
            beam, xy_interval = s.plbeam(
                u, v, uv_index, max_uv_dist, 
                weighting_map[self.beam.get_weighting()],
                self.beam.get_oversampling())
            beam /= 1000
            itf = self.beam.get_image_transfer_function()
            if itf != "Linear":
                min_beam = np.min(beam, axis=None)
                max_beam = np.max(beam, axis=None)
                # normalize to [0, 1]
                beam = (beam - min_beam) / (max_beam - min_beam)
                if itf == "Logarithmic":
                    scale_factor = 65000 # copied from PGPLOT
                    beam = np.log(1 + scale_factor * beam) / \
                           math.log(1 + scale_factor)
                elif itf == "Square-root":
                    beam = np.sqrt((beam - min_beam) / (max_beam - min_beam))
                else:
                    raise RuntimeError("Unknown image transfer function {}".\
                                       format(itf))
                # scale back to [min, max]
                beam = beam * (max_beam - min_beam) + min_beam
            y, x = (np.mgrid[-beam.shape[1]//2:beam.shape[1]//2, 
                            -beam.shape[0]//2:beam.shape[0]//2] 
                    + 0.5) * xy_interval
            # transpose and flip beam to get the same picture as SCHED
            # (cannot find the documentation on meaning of the axes)
            beam = np.flip(beam.T, 0)

            image = axis.pcolormesh(x, y, beam,
                                    cmap=self.beam.get_color_map(),
                                    shading="auto")
            color_bar = figure.colorbar(image, ax=axis)
            if self.beam.get_contours():
                # levels copied from SCHED
                levels = [0.01, 0.03, 0.05, 0.08, 0.1, 0.3, 0.5, 0.8]
                min_beam = np.min(beam, axis=None)
                if min_beam < 0:
                    levels[0:4] = [min_beam * 2 / 3, min_beam / 3,
                                   -min_beam / 3, -min_beam * 2 / 3]
                    # plot negative contours with dashed lines
                    linestyles = ["dashed"] * 2 + ["solid"] * 6
                else:
                    linestyles = ["solid"] * 8
                overlay = axis.contour(x + 0.5 * xy_interval, 
                                       y + 0.5 * xy_interval, 
                                       beam, levels=sorted(set(levels)),
                                       linestyles=linestyles,
                                       cmap=self.beam.get_contour_colors())
                color_bar.add_lines(overlay)
                for line in color_bar.lines:
                    line.set_linewidth(5)
            
            axis.set_aspect("equal")
            axis.xaxis.set_label_text("RA (mas)")
            axis.invert_xaxis()
            axis.yaxis.set_label_text("Dec (mas)")
            axis.set_title("Beam for {} at {}".format(source, wave_text))
            adjust_toolbar(figure)
            figure.tight_layout()


def show(is_restart):
    """
    Return: (make files, do restart)
    """
    app = QApplication([])
    window = MainWidget(is_restart)
    window.setWindowTitle("SCHED Plot")
    window.finished.connect(app.closeAllWindows)
    window.show()
    plt.ion()
    app.exec_()
    r = window.result() # 0 -> exit, 1 -> finish, 2 -> restart
    return (r == 1, r == 2)
