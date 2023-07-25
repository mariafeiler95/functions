// ref https://stackoverflow.com/questions/47236982/imagej-macro-to-open-each-sub-folder-as-a-stack-and-do-something
// ref https://imagej.net/scripting/generic-dialog

// First dialog, basic imput and putput options.
Dialog.create("Process Zooms");
	Dialog.addDirectory("Input Directory", File.directory());
	Dialog.addCheckbox("Scale images before processing?", true);
	Dialog.addCheckbox("Add scale to exported images?", true);
	Dialog.addChoice("Input File Type", newArray(".png", ".jpg", ".tif", ".tiff"));
	Dialog.addChoice("Output File Type", newArray(".png", ".jpg", ".tif", ".tiff"));
Dialog.show();

// Get information from "Process Zooms" dialog.
input = Dialog.getString();
				// Where parent directories are. 
output = input;
							// Where parent directories are. 
scaleinput = Dialog.getCheckbox();
		// true/false, user wants scaling?
printscale = Dialog.getCheckbox();
		// true/false, user wants scale bar?
insuffix = Dialog.getChoice();
			// Expected file format for stack images.
outsuffix = Dialog.getChoice();
			// Desired file format for exported image.

// If scaling is desired, then open a new dialog to get scaling information.
if(scaleinput == true){
	Dialog.create("Set Scale");
		Dialog.addNumber("Pixels", 1);
		Dialog.addToSameRow();Dialog.addNumber("Known distance", 1); 
		Dialog.addToSameRow();Dialog.addChoice("Unit", newArray("nm", "um", "mm", "cm", "meter", "km", "inch", "unit"));
	Dialog.show();
	
	// Get information from "Set Scale" dialog.
	pix = Dialog.getNumber();
			// Number of pixels.
	kno = Dialog.getNumber();
			// Known distance of pixels.
	unit = Dialog.getChoice();
			// Desired unit.
}

// Get information for printing scale bar to the images. Will always do horizontal, bold, and overlay.
if(printscale == true) {
	
	// Check if user is okay with images being flattened if option other than ".tiff" selected for output format.
	if(outsuffix != ".tiff") {
		Dialog.create("Flatten Images?");
			Dialog.addMessage("Your output format choice will cause\nthe scale bar overlay to be flattened,\nwhich might cause issues with later analysis.");
			Dialog.addRadioButtonGroup("Do you wish to switch to a .tiff output?", newArray("Yes", "No"), 1,2,"Yes");
		Dialog.show();
		
		// Reset outsuffix if "yes". Creates dialog box to confirm that overlaying the scale bar is okay.
		if(Dialog.getRadioButton == "Yes") {outsuffix = ".tiff";} else {overlay = "overlay";}
		
		Dialog.create("Flatten Images?");
			Dialog.addMessage("Default saves overlays such as the scale bar to a .tiff file's header.\nThis means if your file is opened in other programs,\nthe scale bar may not show.");
			Dialog.addRadioButtonGroup("Would you prefer to flatten the image?",  newArray("Yes", "No"), 1,2,"No");
		Dialog.show();
		
		if(Dialog.getRadioButton == "No") {overlay = '';}

	// If user picked picked ".tiff" for output images, confirm that they do not want the scale bar overlaid. 
	} else {
		Dialog.create("Flatten Images?");
			Dialog.addMessage("Default saves overlays such as the scale bar to a .tiff file's header.\nThis means if your file is opened in other programs,\nthe scale bar may not show.");
			Dialog.addRadioButtonGroup("Would you prefer to flatten the image?", 
newArray("Yes", "No"), 1,2,"No");
		Dialog.show();
		
		if(Dialog.getRadioButton == "No") {overlay = '';}
	}

	// Get scale bar settings to be passed to run("Scale Bar...")
	Dialog.create("Scale Bar Settings");
		Dialog.addNumber("Width", 1);
		Dialog.addNumber("Height", 1);
		Dialog.addNumber("Thickness", 0.5);
		Dialog.addNumber("Font Size", 10);
		Dialog.addString("Scale Color", "White");
		Dialog.addString("Scale Background", "None");
		Dialog.addChoice("Scale Location", newArray("Lower Right", "Upper Right", "Lower Left", "Lower Right"));
		Dialog.addCheckboxGroup(2, 2, newArray("Horizontal", "Vertical", "Bold", "Hide Label"), newArray(true, false, true, false));
	Dialog.show();

	wid = Dialog.getNumber();
	hei = Dialog.getNumber();
	thi = Dialog.getNumber();
	fs = Dialog.getNumber();
	scol = Dialog.getString();
	bgcol = Dialog.getString();
	loc = Dialog.getChoice();
	if(Dialog.getCheckbox() == true) {horz = "horizontal";} else {horz = '';}
	if(Dialog.getCheckbox() == true) {vert = "vertical";} else {vert = '';}
	if(Dialog.getCheckbox() == true) {bold = "bold";} else {bold = '';}
	if(Dialog.getCheckbox() == true) {hide = "hide";} else {hide = '';}
}

// Get Activity Log text window set up 
list = getList("window.titles");
f = "[Activity Log]";

if(list.length == 0){
	run("Text Window...", "name=[Activity Log] width=80 height=24");
}

if(list.length >= 1){
	for(i=0; i<list.length; i++){
		if(list[i] == "Activity Log"){print("[Activity Log]", "\\Update:");}
	}
}

// Starting Activity Log
print(f, "Processing " + input + "\n");
print(f, "Input files of " + insuffix + " format.\n");
print(f, "Output files of " + outsuffix + " format.\n");
if(ali == true){print(f, "Aligned stacks will also be saved.\n");}
if(scaleinput == true){
	print(f, "Output scale: " + pix + " pixels per " + kno + " " + unit + ".\n");
}
if(printscale == true) {
	print(f, "Scale added to output images.\n");
}

processFolder(input);

// Function to sort through directories, search for subdirectories, then apply processing procedure to files within the subdirectories. 
function processFolder(input) {
	list = getFileList(input);
	list = Array.sort(list);
	for (i = 0; i < list.length; i++) {
		if(File.isDirectory(input + File.separator + list[i])) {
			outputdir = input;
			processFolder(input + File.separator + list[i]);
		}
 
		if(endsWith(list[i], insuffix)) {
			inputdir = input;
			processFile(input, output, list[i]);
			i = list.length-1;
		}
	}
}

// Function to open image sequence within subdirectory, run Set Scale, StackReg, Stack Focuser, and Scale Bar if applied, then save the transformed image to the parent directory.
// Robust against subdirectories that are empty (moves on) or that only contain one image (scales/adds scale bar if requested, saves file without any stack alignment or focusing.)
function processFile(input, output, file) {
	wait(2000);
	print(f, "\n" + inputdir + "\n");

	// Open file sequence and get information
	File.openSequence(input);
	title=getTitle;
   	nz=nSlices;
   	nk = nz + 1;				//Necessary if image sequence is only 2 stacks.
	selectWindow(title);
	
	if(nz == 1){
		print(f, "       " + inputdir + " only contains one image.\n");
		
		// Scale if chosen.
		if(scaleinput == true) {
			run("Set Scale...", "distance=pix known=kno unit=unit global");
		}
		// Print scale bar if chosen.
		if(printscale == true) {
			run("Scale Bar...", "width=wid height=hei thickness=thi font=fs color=&scol background=&bgcol location=[&loc] &horz &vert &bold &hide &overlay");
		}
		
		// Save file.
		print(f, "       Saved as " + title + outsuffix + "\n");
		selectWindow(title);
		save(outputdir+title+outsuffix);

		close("*");
	}
	
	else {
		print(f,  "       Stack contains " + nz + " images.\n");
		
		// Scale if chosen.
		if(scaleinput == true) {
			run("Set Scale...", "distance=pix known=kno unit=unit global");
		}

		print(f, "       Linear Stack Alignment with SIFT: Running...");
		run("Linear Stack Alignment with SIFT", "initial_gaussian_blur=1.60 steps_per_scale_octave=3 minimum_image_size=64 maximum_image_size=1024 feature_descriptor_size=4 feature_descriptor_orientation_bins=8 closest/next_closest_ratio=0.92 maximal_alignment_error=25 inlier_ratio=0.05 expected_transformation=Rigid interpolate");
		print(f, " Complete!\n");
	  
    selectWindow("Aligned " + nz + " of " + nz);
		print(f, "       StackFocuser: Running...");
		run("Stack Focuser ", "enter=nk");
		print(f, " Complete!\n");
	
		// Print scale bar if chosen.
		if(printscale == true) {
			run("Scale Bar...", "width=wid height=hei thickness=thi font=fs color=&scol background=&bgcol location=[&loc] &horz &vert &bold &hide &overlay");
		}

		// Save file.
		print(f, "       Saved as " + title + outsuffix + "\n");
		save(outputdir+title+outsuffix);

		close("*");
	}
}
