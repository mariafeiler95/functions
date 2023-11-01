# Maria Feiler's Functions

This is a collection of R functions that I have created or modified and ImageJ macros. 

All packages, authors, or help pages who served as inspiration or provided code will be cited within the function source code files. 

Using/modifying my functions/macros? Feel free! But please cite me if necessary and let me know of any improvements you make! I am always open for suggestions. 

As of 11/01/2023:
| R function             | description                                                                               | functional?             |
|------------------------|-------------------------------------------------------------------------------------------|-------------------------|
| packageDocumentation.R | Given one or more package names and a date, extract the package version and authors.      | yes                     |
| mrkJsonToArray.R       | Read one or more .mrk.json landmark files into array.                                     | yes                     |
| plotRefTwoTargets.R    | Plot three dimensional shape differences between a reference and target.                  | yes                     |
| procDistCIs.R          | Calculate Procrustes distance between two arrays of landmark data.                        | yes                     |
| mShapeDists.R          | Calculate the Procrustes distances of landmark data within an array from the mean shape.  | yes                     |
| Walker2008.R           | Estimate sex based on cranial nonmetric trait scores using Walker 2008.                   | yes                     |
| Walker2005.R           | Estimate sex based on greater sciatic notch scores using Walker 2005.                     | yes                     |
| Klales2012.R           | Estimate sex based on pubic trait scores using Klales et al. 2012.                        | yes                     |
| calcLD.R               | Calculate linear distance between two 3D landmarks across an array.                       | yes                     |
| midlineLM.R            | Calculate midline landmark between two 3D landmarks across an array.                      | yes                     |
| bulkWriteMarkups.R     | Write .fcsv files for each specimen in a sample.                                          | yes                     |

  
| ImageJ macro           | description                                                                               | functional?                 |
|------------------------|-------------------------------------------------------------------------------------------|-----------------------------|
| Process_Confocal.ijm   | Load, smooth, enhance contrast, and export confocal images.                               | yes                         |
| Process_Zooms.ijm      | Load, register, focus, scale, add scale bars, and export microscope image stacks.         | flattening when not told to |
