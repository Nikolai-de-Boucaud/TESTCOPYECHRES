import sys
import MDSplus
import matplotlib
import matplotlib.pyplot as plt
import ctypes 
from matplotlib.backends.backend_qt5agg import FigureCanvasQTAgg as FigureCanvas
from matplotlib.backends.backend_qt5agg import NavigationToolbar2QT as NavigationToolbar
from matplotlib.figure import Figure
from PyQt5.QtWidgets import *
from PyQt5.QtGui import *
from PyQt5 import QtCore

class DensityGUI(QWidget):
    def __init__(self):
        super().__init__()

        #set window title
        self.setWindowTitle("ECH Density Limit Calculator\n")
        
        # Create the widgets
        self.title = QLabel("ECH Density Limit Calculator")
        self.shot_number_label = QLabel("Shot Number:")
        self.shot_number_label.setMaximumWidth(130)
        self.shot_number_input = QLineEdit()
        self.shot_number_input.setMaximumWidth(200)
        self.use_pcs_cbox = QCheckBox("Use Auto-Calculated Density")
        self.use_pcs_cbox.setChecked(True)
        self.use_custom_cbox = QCheckBox("Find Peak Density Within Times:")
        self.use_custom_cbox.setChecked(False)
        self.calculate_button = QPushButton("Find Peak Density")
        self.calculate_button.setFont(QFont("Arial", 16))
        self.time_slice_output = QPlainTextEdit()
        self.time_slice_output.setReadOnly(True)
        self.time_slice_output.setFixedHeight(30)
        self.reviewplus_density_output = QPlainTextEdit()
        self.reviewplus_density_output.setReadOnly(True)
        self.reviewplus_density_output.setFixedHeight(30)
        self.zipfits_density_output = QPlainTextEdit()
        self.zipfits_density_output.setReadOnly(True)
        self.zipfits_density_output.setFixedHeight(30)
        self.reflection_density_output = QPlainTextEdit()
        self.reflection_density_output.setReadOnly(True)
        self.reflection_density_output.setFixedHeight(30)
        self.calculated_density_limit_output = QPlainTextEdit()
        self.calculated_density_limit_output.setReadOnly(True)
        self.calculated_density_limit_output.setFixedHeight(30)

        # Create the Matplotlib Figure and FigureCanvas widgets
        self.figure = Figure()
        self.canvas = FigureCanvas(self.figure)

        # Create the toolbar
        toolbar = NavigationToolbar(self.canvas, self)

        #Set up the font for the title
        title_font = QFont("Arial", 18)
        title_font.setBold(True)
        self.title.setFont(title_font)
        self.title.setAlignment(QtCore.Qt.AlignCenter)
        
        # Set up the font for the labels and input field
        label_font = QFont("Arial", 14)
        self.shot_number_label.setFont(label_font)
        self.shot_number_input.setFont(label_font)

        # Set up the font for the output fields
        output_font = QFont("Arial", 12)
        self.time_slice_output.setFont(output_font)
        self.reviewplus_density_output.setFont(output_font)
        self.zipfits_density_output.setFont(output_font)
        self.reflection_density_output.setFont(output_font)
        self.calculated_density_limit_output.setFont(output_font)

        spacer = QSpacerItem(100, 200, QSizePolicy.Minimum, QSizePolicy.Expanding)

        # Set up the layout
        input_layout = QHBoxLayout()
        input_layout.addWidget(self.shot_number_label)
        input_layout.addWidget(self.shot_number_input)
        input_layout.setAlignment(QtCore.Qt.AlignCenter)

        pcs_box_layout = QHBoxLayout()
        pcs_box_layout.addWidget(self.use_pcs_cbox)
        pcs_box_layout.addSpacerItem(spacer)
        pcs_box_layout.addWidget(self.use_custom_cbox)
        pcs_box_layout.setAlignment(QtCore.Qt.AlignCenter)

        output_layout = QVBoxLayout()
        output_layout.addWidget(QLabel("Time (ms):"))
        output_layout.addWidget(self.time_slice_output)
        output_layout.addWidget(QLabel("Max Line Avg Density (cm⁻³):"))
        output_layout.addWidget(self.reviewplus_density_output)
        output_layout.addWidget(QLabel("Zipfits Core Density (cm⁻³):"))
        output_layout.addWidget(self.zipfits_density_output)
        output_layout.addWidget(QLabel("Reflection Core Density (cm⁻³):"))
        output_layout.addWidget(self.reflection_density_output)
        output_layout.addWidget(QLabel("Calculated Density Limit (cm⁻³):"))
        output_layout.addWidget(self.calculated_density_limit_output)

        button_layout = QHBoxLayout()
        button_layout.addWidget(self.calculate_button)

        main_layout = QVBoxLayout()
        main_layout.addWidget(self.title)
        main_layout.addSpacerItem(spacer)
        main_layout.addLayout(input_layout)
        main_layout.addSpacerItem(spacer)
        main_layout.addLayout(pcs_box_layout)
        main_layout.addSpacerItem(spacer)
        main_layout.addLayout(button_layout)
        main_layout.addSpacerItem(spacer)
        main_layout.addLayout(output_layout)
        main_layout.addSpacerItem(spacer)
        
        # Add the FigureCanvas widget to the layout
        main_layout.addWidget(self.canvas)
        main_layout.addWidget(toolbar)

        # Connect the signal and slot
        self.calculate_button.clicked.connect(self.calculate_density)

        # Set the main layout of the window
        self.setLayout(main_layout)

        # Set the window size and font
        self.setGeometry(100 ,500, 600, 900)
        self.setFont(label_font)


    def plot_data(self, shotnum):
        # Perform the calculation to get the data to plot
        c = MDSplus.Connection("atlas.gat.com")

        y = c.get('PTDATA("ecsdensf",' + str(shotnum) + ')')
        x = c.get('DIM_OF(PTDATA("ecsdensf",' + str(shotnum) + '))')

        # Clear the previous plot
        self.figure.clear()

        # Create a subplot and plot the data
        ax = self.figure.add_subplot(111)
        ax.plot(x, y)

        # Set the title and axis labels
        ax.set_title(f"Shot Number: {shotnum}")
        ax.set_xlabel("Time (ms)")
        ax.set_ylabel("Line Avg Density (cm⁻³)")
        ax.set_xbound(-1000,8000)
        self.figure.tight_layout()

        # Refresh the canvas to show the new plot
        self.canvas.draw()
    

    def calculate_density(self):  
        try:
            # Get the input value
            shot_number = self.shot_number_input.text()

            # Do the calculation and update the output values
            # Replace this with your actual calculation
            time_slice = 13000
            reviewplus_density = 12000
            zipfits_density = 8000
            reflection_density = 30000
            calculated_density_limit = reflection_density*(reviewplus_density/zipfits_density)

            # Update the output fields
            self.time_slice_output.setPlainText(str(time_slice))
            self.reviewplus_density_output.setPlainText(str(reviewplus_density))
            self.zipfits_density_output.setPlainText(str(zipfits_density))
            self.reflection_density_output.setPlainText(str(reflection_density))
            self.calculated_density_limit_output.setPlainText(str(calculated_density_limit))

            # Call the plot_data method to plot the data
            self.plot_data(shot_number)
        
        except:
            ctypes.windll.user32.MessageBoxW(0, "Error Retrieving Shot Data.", "Error Message", 1)

if __name__ == '__main__':
    app = QApplication(sys.argv)
    density_gui = DensityGUI()
    density_gui.show()
    sys.exit(app.exec_())

