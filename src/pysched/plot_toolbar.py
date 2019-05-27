"""
Module to update the toolbar of matplotlib plots to work around a few 
issues specific to pySCHED (no proper date editor, as the get_[xy]lim function 
returns a float, edit all 3 axis at the same time, LST limits need their own 
widget, ...).

The main function is adjust. This will work on the toolbar after it is created. 
It will remove a widget and its action and insert a new one. A similar approach 
is taken with the the resulting popup dialog (remove and insert). The 
alternative is to subclass the toolbar and dialog classes, but that would 
cause a lot of code copy-pasting. Even with this approach there is some 
copy-pasting, therefor the matplotlib license:

License Agreement (MIT License)
------------------------------------------

Copyright (c) 2009 Pierre Raybaut

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
"""
import matplotlib.backends.qt_editor.figureoptions as figureoptions
import matplotlib.dates
import six
from matplotlib.colors import rgb2hex
from matplotlib.colors import colorConverter

# hack around bug in formlayout, set an environment variable to force use of Qt5
import os; os.environ["QT_API"] = "pyqt5"
import formlayout

from PyQt5.QtWidgets import QWidget, QTimeEdit, QMessageBox, QHBoxLayout, \
    QSpinBox, QAction
from PyQt5.QtCore import QDateTime

from datetime import datetime, timedelta

from distutils.version import LooseVersion
from PyQt5.Qt import PYQT_VERSION_STR
if LooseVersion(PYQT_VERSION_STR) < LooseVersion("5.8"):
    raise ImportError("PyQt version 5.8 or higher is required")

class LstWidget(QWidget):
    def __init__(self, value, lst_base_date, parent=None):
        super().__init__(parent)
        layout = QHBoxLayout(self)
        self.lst_base_date = lst_base_date
        # offset to lst base edit
        self.days = QSpinBox(self)
        self.days.setMinimum(-10) # somewhat arbitrary value
        self.days.setValue((value.date() - lst_base_date).days)
        layout.addWidget(self.days)
        self.time = QTimeEdit(self)
        self.time.setDisplayFormat("hh:mm:ss")
        self.time.setTime(value.time())
        layout.addWidget(self.time)

    def dateTime(self):
        # will be called by .get of the dialog, expecting a QDateTime return
        return QDateTime(datetime.combine(self.lst_base_date, 
                                          self.time.time().toPyTime()) +
                         timedelta(days=self.days.value()))


def edit_parameters(toolbar, x_axis_type):
    allaxes = toolbar.canvas.figure.get_axes()
    if not allaxes:
        QMessageBox.warning(
            toolbar.parent, "Error", "There are no axes to edit.")
        return

    # edit all axes at the same time, 
    # only makes sense if same data shape is edited
    axis = allaxes[-1]
    sep = (None, None)  # separator

    has_curve = len(axis.get_lines()) > 0

    # Get / General
    if x_axis_type in ["GST", "LST"]:
        xmin, xmax = (matplotlib.dates._from_ordinalf(l) 
                      for l in axis.get_xlim())
    elif x_axis_type == "UT":
        xmin, xmax = (matplotlib.dates._from_ordinalf(l) 
                      for l in axis.get_xlim())
    else:
        xmin, xmax = axis.get_xlim()
    ymin, ymax = axis.get_ylim()

    general = [('Title', axis.get_title()),
               sep,
               (None, "<b>X-Axis</b>"),
               ('Min', xmin), 
               ('Max', xmax),
               ('Scale', [axis.get_xscale(), 'linear', 'log']),
               sep,
               (None, "<b>Y-Axis</b>"),
               ('Min', ymin), 
               ('Max', ymax),
               ('Scale', [axis.get_yscale(), 'linear', 'log']),
               sep,
               ('(Re-)Generate automatic legend', False),
               ]
    xmin_row = 3
    xmax_row = 4

    # Save the unit data
    xconverter = axis.xaxis.converter
    yconverter = axis.yaxis.converter
    xunits = axis.xaxis.get_units()
    yunits = axis.yaxis.get_units()
    xformatter = axis.xaxis.get_major_formatter()
    yformatter = axis.yaxis.get_major_formatter()

    if has_curve:
        # Get / Curves
        linedict = {}
        for line in axis.get_lines():
            label = line.get_label()
            if label == '_nolegend_':
                continue
            linedict[label] = line
        curves = []
        linestyles = list(six.iteritems(figureoptions.LINESTYLES))
        drawstyles = list(six.iteritems(figureoptions.DRAWSTYLES))
        markers = list(six.iteritems(figureoptions.MARKERS))
        curvelabels = sorted(linedict.keys())
        for label in curvelabels:
            line = linedict[label]
            color = rgb2hex(colorConverter.to_rgb(line.get_color()))
            ec = rgb2hex(colorConverter.to_rgb(line.get_markeredgecolor()))
            fc = rgb2hex(colorConverter.to_rgb(line.get_markerfacecolor()))
            curvedata = [('Label', label),
                         sep,
                         (None, '<b>Line</b>'),
                         ('Line Style', [line.get_linestyle()] + linestyles),
                         ('Draw Style', [line.get_drawstyle()] + drawstyles),
                         ('Width', line.get_linewidth()),
                         ('Color', color),
                         sep,
                         (None, '<b>Marker</b>'),
                         ('Style', [line.get_marker()] + markers),
                         ('Size', line.get_markersize()),
                         ('Facecolor', fc),
                         ('Edgecolor', ec),
                         ]
            curves.append([curvedata, label, ""])

        # make sure that there is at least one displayed curve
        has_curve = bool(curves)

    datalist = [(general, "Axes", "")]
    if has_curve:
        datalist.append((curves, "Curves", ""))

    def apply_callback(data):
        """This function will be called to apply changes"""
        if has_curve:
            general, curves = data
        else:
            general, = data

        # Set / General
        title, xmin, xmax, xscale, ymin, ymax, yscale, \
            generate_legend = general

        for axis in allaxes:
            axis.set_xscale(xscale)
            axis.set_yscale(yscale)
            axis.set_title(title)
            axis.set_xlim(xmin, xmax)
            axis.set_ylim(ymin, ymax)

            # Restore the unit data
            axis.xaxis.converter = xconverter
            axis.yaxis.converter = yconverter
            axis.xaxis.set_units(xunits)
            axis.yaxis.set_units(yunits)
            axis.xaxis.set_major_formatter(xformatter)
            axis.yaxis.set_major_formatter(yformatter)
            axis.xaxis._update_axisinfo()
            axis.yaxis._update_axisinfo()

            if has_curve:
                # Set / Curves
                for index, curve in enumerate(curves):
                    line = linedict[curvelabels[index]]
                    label, linestyle, drawstyle, linewidth, color, \
                        marker, markersize, markerfacecolor, markeredgecolor \
                        = curve
                    line.set_label(label)
                    line.set_linestyle(linestyle)
                    line.set_drawstyle(drawstyle)
                    line.set_linewidth(linewidth)
                    line.set_color(color)
                    if marker != 'none':
                        line.set_marker(marker)
                        line.set_markersize(markersize)
                        line.set_markerfacecolor(markerfacecolor)
                        line.set_markeredgecolor(markeredgecolor)

            # re-generate legend, if checkbox is checked

            if generate_legend:
                draggable = None
                ncol = 1
                if axis.legend_ is not None:
                    old_legend = axis.get_legend()
                    draggable = old_legend._draggable is not None
                    ncol = old_legend._ncol
                new_legend = axis.legend(ncol=ncol)
                if new_legend:
                    new_legend.draggable(draggable)

        # Redraw
        figure = axis.get_figure()
        figure.canvas.draw()

    dialog = formlayout.FormDialog(
        datalist, 
        title="Figure options", 
        icon=figureoptions.get_icon("qt4_editor_options.svg"), 
        apply=apply_callback)
    axes_tab = dialog.formwidget.widgetlist[0]
    if x_axis_type in ["GST", "LST"]:
        layout = axes_tab.formlayout
        for (value, row) in ((xmin, xmin_row), (xmax, xmax_row)):
            layout.removeRow(row)
            widget = LstWidget(value, axis.xaxis.lst_base_date, axes_tab)
            layout.insertRow(row, general[row][0], widget)
            axes_tab.widgets[row] = widget
    elif x_axis_type == "UT":
        for row in (xmin_row, xmax_row):
            axes_tab.widgets[row].setDisplayFormat("yyyy-MM-dd hh:mm:ss")
    
    if dialog.exec_():
        apply_callback(dialog.get())


def adjust(figure, x_axis_type):
    # rebind configure action, a bit of a hack, but prevents copying a lot
    # of code from matplotlib
    toolbar = figure.canvas.manager.toolbar
    before = None
    for action in toolbar.actions()[::-1]:
        if action.text() == "Customize":
            toolbar.removeAction(action)
            if before is not None:
                new_action = QAction(action.icon(), action.text(), toolbar)
                new_action.triggered.connect(
                    lambda: edit_parameters(toolbar, x_axis_type))
                toolbar.insertAction(before, new_action)
            else:
                toolbar.addAction(action.icon(), action.text(), 
                                  lambda: edit_parameters(toolbar, x_axis_type))
            break
        before = action
