// This macro was used to process confocal images that had already been separated into three channels. 
// User defines their directories, input and output file types, and any scaling or image properties. 

Dialog.create("Process Confocal");
	Dialog.addDirectory("Input Directory", File.directory());
	Dialog.addDirectory("Output Directory", File.directory());
	Dialog.addChoice("Units", newArray("cm", "mm", "µm", "m", "inch", "unit"));
	Dialog.addNumber("Pixel Width", 1); 
	Dialog.addToSameRow(); Dialog.addNumber("Pixel Height", 1);
	Dialog.addToSameRow(); Dialog.addNumber("Voxel Depth", 1);
	Dialog.addNumber("Scale X", 1); 
	Dialog.addToSameRow(); Dialog.addNumber("Scale Y", 1);
	Dialog.addToSameRow(); Dialog.addNumber("Scale Z", 1);
	Dialog.addChoice("Input file format", newArray(".tif", ".tiff", ".gif", ".bmp"));
	Dialog.addChoice("Output file format", newArray(".tif", ".tiff", ".gif", ".bmp"));
Dialog.show();

// Get dialog selections
input = Dialog.getString();
output = Dialog.getString();
un = Dialog.getChoice();
pw = Dialog.getNumber();
ph = Dialog.getNumber();
vd = Dialog.getNumber();
sx = Dialog.getNumber();
sy = Dialog.getNumber();
sz = Dialog.getNumber();
insuffix = Dialog.getChoice();
outsuffix = Dialog.getChoice();

// Get Activity Log text window set up 
list = getList("window.titles");
f = "[Activity Log]";

if(list.length == 0){run("Text Window...", "name=[Activity Log] width=80 height=24");}

if(list.length >= 1){
	for(i=0; i<list.length; i++){
		if(list[i] == "Activity Log"){print("[Activity Log]", "\\Update:");}
	}
}

// Starting Activity Log
print(f, "Processing " + input + "\n");
print(f, "Output directory: " + output + "\n");

list = getFileList(input);
list = Array.sort(list);
for (i = 0; i < list.length; i++) {
	processFile(input, list[i]);
}

function processFile(input, file) {
	print(f, "\n" + file + "\n");
	
	open(input + file);
	nz = nSlices;
	outtitle = replace(file, insuffix, "") + "_scaled";
	
	selectWindow(file);
	print(f, "       Changed properties. ");
	run("Properties...", "channels=1 slices=nz frames=1 unit=un pixel_width=pw pixel_height=ph voxel_depth=vd");
	
	print(f, "Enhanced contrast. ");
	run("Enhance Contrast", "saturated=0.35");
	run("Apply LUT", "stack");

	print(f, "Converted to 8-bit. ");
	run("8-bit");
	
	print(f, "Set scale. ");
	run("Scale...", "x=sx y=sy z=sz interpolation=Bicubic average process create title=outtitle");
	
	selectWindow(outtitle);
	print(f, "\n       Saved as " + outtitle + outsuffix + "\n");
	save(output + outtitle + outsuffix);
	close("*");
}

