/* The following method PaneloptProviderImpl.java ilustrates the implementation of
the EBF3PanelOpt framework as an analysis provider. The method illustrates the use of
GenericUtil for script robustness.
*/

/*
 * Distribution Statement
 * 
 * This computer software has been developed under sponsorship of the United States Air Force Research Lab. Any further
 * distribution or use by anyone or any data contained therein, unless otherwise specifically provided for,
 * is prohibited without the written approval of AFRL/RQVC-MSTC, 2210 8th Street Bldg 146, Room 218, WPAFB, OH  45433
 * 
 * Disclaimer
 * 
 * This material was prepared as an account of work sponsored by an agency of the United States Government. Neither
 * the United States Government nor the United States Air Force, nor any of their employees, makes any warranty,
 * express or implied, or assumes any legal liability or responsibility for the accuracy, completeness, or usefulness
 * of any information, apparatus, product, or process disclosed, or represents that its use would not infringe privately
 * owned rights.
 */
package mil.afrl.mstc.open.panelopt.provider;
import com.sun.jini.start.LifeCycle;
import mil.afrl.mstc.open.panelopt.Panelopt;
import mil.afrl.mstc.open.panelopt.PaneloptContext;
import mil.afrl.mstc.open.core.provider.EngineeringProvider;
import net.jini.config.ConfigurationException;
import sorcer.service.Context;
import sorcer.util.GenericUtil;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.Vector;
/*
Implementation of the {mil.afrl.mstc.open.panelopt.Panelopt} interface.
@author MSTC Engineering Project Generator
*/
public class PaneloptProviderImpl extends EngineeringProvider implements
Panelopt {
	private final static Logger logger =
	Logger.getLogger(PaneloptProviderImpl.class.getName());
/*
Default constructor initializes the provider with the properties file
loaded as a resource.
@throws Exception if initialization fails.
*/
	public PaneloptProviderImpl() throws Exception {
		this("panelopt/provider.properties");
	}
	
	/*
	Create the provider using a properties file
	@param propFile The properties file, must not be {@code null}.
	@throws Exception If initialization fails.
	*/
	public PaneloptProviderImpl(final String propFile) throws Exception {
		super(propFile);
		initialize();
	}
	
	/*
	Create the provider if called from the River {@link
	com.sun.jini.start.ServiceStarter} approach.
	@param args: Parameters that point to configuration file and possible
	overrides. Never {@code null}.
	@param lifeCycle: The lifecycle container to be notified if the
	provider terminates.
	@throws Exception if initialization fails.
	*/
	public PaneloptProviderImpl(final String[] args, final LifeCycle lifeCycle)
	throws Exception {
		super(args, lifeCycle);
		initialize();
	}
	
	/* Provides provider specific initialization and environment checking */
	private void initialize() throws ConfigurationException {
		/* Provides support for JavaSpaces */
		initSpaceSupport();
	}
	public Context execute(final Context context) {
		long startTime = System.currentTimeMillis();
		logger.info("Running execute");
		File scratchDir = null;
		URL scratchUrl = null;
		String serviceIdString = doThreadMonitor(null);
		try {
		PaneloptContext paneloptContext = (PaneloptContext) context;
		logger.info("Obtaining scratch directory ... ");
		/* Obtain scratch directory. */
		scratchDir = getScratchDir(context, "panelopt_execute_");
		logger.info(String.format("Scratch directory = [%s]",
		scratchDir.getAbsolutePath()));
		scratchUrl = getScratchURL(scratchDir);
		logger.info(String.format("Scratch URL = [%s]", scratchUrl));
		/* Get the input file and write it out locally. */
		URL paneloptInputURL = (URL) paneloptContext.getInput0();
		File paneloptInputFile = new File(scratchDir, "stiffened_plate_input_data.txt");
		GenericUtil.download(paneloptInputURL, paneloptInputFile);
		/* Write design points to a file. */
		File paneloptDesignVarsFile = new File(scratchDir, "dvar.vef");
		if (!paneloptDesignVarsFile.exists()) {
			paneloptDesignVarsFile.createNewFile();
		}
		
		/* Get system property. */
		String paneloptExecProp = null;
		if (GenericUtil.isLinux()) 
			paneloptExecProp = getProperty("provider.exec.panelopt.linux64").trim();
		if (GenericUtil.isMac()) 
			paneloptExecProp = getProperty("provider.exec.panelopt.mac64").trim();
		File panelOptExec = new File(paneloptExecProp);
		File panelOptExecHome = panelOptExec.getParentFile();
		
		/* Get scratch directory. */
		Vector<String> script = new Vector<String>();
		script.add("#!/bin/bash");
		script.add("cd " + scratchDir.getAbsolutePath());
		script.add("cp " + panelOptExecHome.getAbsolutePath() + "/* .");
		script.add("./" + panelOptExec.getName());
		
		/* Get scratch directory. */
		File panelOptScriptFile = new File(scratchDir, "doIt.sh");
		File panelOptLogFile = new File(scratchDir,
		panelOptScriptFile.getName() + "Log.txt");
		
		/* Run python script */
		int exitStatus = 1;
		long lStartTime = System.currentTimeMillis();
		exitStatus = GenericUtil.runShellScript(panelOptScriptFile,
		script, panelOptLogFile, 0, false, true, true);
		logger.info("finished executing script via systemCall...exitValue = " + exitStatus);
		
		/* Get objective function values from file */
		File panelOptoutfile1 = new File(scratchDir, "resp.vef");
		Vector<String> outputFileContents =
		GenericUtil.getFileContents(panelOptoutfile1);
		Double[] outputArray = new Double[outputFileContents.size()];
		int i = 0;
		for (String value : outputFileContents) {
			outputArray[i++] = new Double(value);
		}
		logger.info("outputArray = " + outputArray);
		paneloptContext.outputArray = outputArray;
		logger.info("panelOptContext = " + paneloptContext);
		} catch (Exception e) {
			StringBuilder reason = new StringBuilder();
			reason.append("error: PaneloptProviderImpl caught exception = ").append(e.getClass().getName()).append("scratchDir = ").append(scratchDir) .append("scratchUrl= ").append(scratchUrl);
			context.reportException(reason.toString(), e,
			getProviderInfo("execute"));
			logger.log(Level.SEVERE, reason.toString(), e);
			return context;
		} finally {
			doThreadMonitor(serviceIdString);
			doTimeKeeping((System.currentTimeMillis() - startTime) / 1000);
		}
		logger.info("Returning from service execute; Output context = "+context); return context;
		return context;
	}

	private void setFileContents(File file, Object[] objA) throws IOException
	{
		Vector<String> vect = new Vector<String>();
		for (Object o : objA) {
			vect.add(o.toString());
		}
		GenericUtil.setFileContents(file, vect);
	}
}
