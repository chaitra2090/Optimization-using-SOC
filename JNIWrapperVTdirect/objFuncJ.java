/* The following program objfuncJ.java contains the objective function for the subroutines
VTdirect, pVTdirect, QNSTOPS, and QNSTOPP. */

import java.io.*;
import java.lang.*;
import java.util.*;

public class objfuncJ {
	public static double evaluate(double[] designPoints) {
		/* Local variables */
		double mass =0;
		File desVars = null;
		File respVars = null;
		try {
			String desVarsFilename = "dvar" + UUID.randomUUID() + ".vef";
			desVars = new File(desVarsFilename);
			String respVarsFilename = "resp" + UUID.randomUUID() + ".vef";
			respVars = new File(respVarsFilename);
			/* Create files for input to the EBF3PanelOpt framework. */
			if (!desVars.exists()) {
				desVars.createNewFile();
			}
			if (!respVars.exists()) {
				respVars.createNewFile();
			}
			FileWriter desVarsWriter = new
			FileWriter(desVars.getAbsoluteFile());
			BufferedWriter bw = new BufferedWriter(desVarsWriter);
			/* Write design points to a file. */
			for(int k=0; k<designPoints.length; k++) {
				bw.write(""+String.format("%.12f",designPoints[k]));
			}
			bw.close();
		} catch (IOException e) {
			e.printStackTrace();
			System.exit(-1);
		}
		try {
			/* System call to the EBF3PanelOpt framework in Python. */
			Process panelOpt = Runtime.getRuntime().exec("python
			evaluate_response.py -i stiffened_plate_input_data.txt -idv "+ desVars + " -o " + respVars);
			panelOpt.waitFor();
		} catch (Exception e) {
			e.printStackTrace();
			System.exit(-1);
		}
		try {
			if (!respVars.exists()) {
			System.out.println("ERROR");
			System.exit(-1);
		}
		
		/* Read response from file. */
		FileReader respVarsReader = new
		FileReader(respVars.getAbsoluteFile());
		BufferedReader br = new BufferedReader(respVarsReader);
		mass = br.readLine();
		} catch (IOException e) {
			e.printStackTrace();
			System.exit(-1);
		}
	/* Return response to glue code in C. */
	return mass;
	}
}
