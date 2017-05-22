/* The following method PaneloptProviderImpl.java ilustrates the implementation of
the EBF3PanelOpt framework as an analysis provider. This implementation illustrates the
use of function calls to the corresponding python script and does not incorporate script
robustness.
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
import mil.afrl.mstc.open.panelopt.Wingopt;
import mil.afrl.mstc.open.panelopt.WingoptContext;
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

/**
 * Implementation of the {@link mil.afrl.mstc.open.panelopt.Wingopt} interface
 *
 * @author MSTC Engineering Project Generator
 */
public class WingoptProviderImpl extends EngineeringProvider implements Wingopt {
    private final static Logger logger = Logger.getLogger(WingoptProviderImpl.class.getName());

    /**
     * Default constructor, initializes the provider with the properties file loaded as a resource.
     *
     * @throws Exception If initialization fails.
     */
    public WingoptProviderImpl() throws Exception {
        this("panelopt/provider.properties");
    }

    /**
     * Create the provider using a properties file
     *
     * @param propFile The properties file, must not be {@code null}
     * @throws Exception If initialization fails.
     */
    public WingoptProviderImpl(final String propFile) throws Exception {
        super(propFile);
        initialize();
    }

    /**
     * Create the provider if called from the River {@link com.sun.jini.start.ServiceStarter} approach.
     *
     * @param args      Parameters that point to configuration file and possible overrides. Never {@code null}.
     * @param lifeCycle The lifecycle container to be notified if the provider terminates.
     * @throws Exception If initialization fails.
     */
    public WingoptProviderImpl(final String[] args, final LifeCycle lifeCycle) throws Exception {
        super(args, lifeCycle);
        initialize();
    }

    /*
     * Provides provider specific initialization and environment checking
     */
    private void initialize() throws ConfigurationException {
        initSpaceSupport();
        if (GenericUtil.isLinux()) panelOptExec = new File(getProperty("provider.exec.panelopt.linux64").trim());
        if (GenericUtil.isMac()) panelOptExec = new File(getProperty("provider.exec.panelopt.mac64").trim());
        panelOptNativeDir = panelOptExec.getParentFile();
        logger.info("panelOptNativeDir = " + panelOptNativeDir);
    }

    private File panelOptExec = null;
    private File panelOptNativeDir = null;
    private static int threadsRunning = 0;

    public Context execute(final Context context) {

        long startTime = System.currentTimeMillis();
        logger.info("\n************* running execute *****************");
        logger.info("incremented threadsRunning = " + ++threadsRunning);

        File scratchDir = null;
        URL scratchUrl = null;

        WingoptContext paneloptContext = (WingoptContext) context;


        try {
        	// Obtain scratch directory 
            scratchDir = getScratchDir(context, "panelopt_execute_");
            logger.info("scratchDir = " + scratchDir.getAbsolutePath());
            scratchUrl = getScratchURL(scratchDir);
            logger.info("scratchUrl = " + scratchUrl);

			// Write design points to a file
            File paneloptDesignVarsFile = new File(scratchDir, "dvar.vef");
            setFileContents(paneloptDesignVarsFile, paneloptContext.dvArray);

			// Create script file to run EBF3PanelOpt
            File panelOptScriptFile = new File(scratchDir, "doIt.sh");
            File panelOptLogFile = new File(scratchDir, panelOptScriptFile.getName() + "Log.txt");
            panelOptScriptFile.createNewFile();
            panelOptScriptFile.setExecutable(true, false);
            FileWriter fw = new FileWriter(panelOptScriptFile, true); //the true will append the new data
            fw.write("#!/bin/bash\n");
            fw.write("cd " + scratchDir.getAbsolutePath() + "\n");
            fw.write("cp " + panelOptNativeDir.getAbsolutePath() + "/* . \n");
            fw.write("./" + panelOptExec.getName() + " &> " + " stdOutAndError.txt \n");
            fw.close();
            logger.info("calling: sh -c " + panelOptScriptFile.getAbsolutePath());
            long sysCallStartTime = System.currentTimeMillis();
    
    		// Execute Script
            Process p = Runtime.getRuntime().exec(new String[]{"sh", "-c", panelOptScriptFile.getAbsolutePath()});
            int exitValue = p.waitFor();
            long sysCallElapsedTimeMilliSeconds = ((System.currentTimeMillis() - sysCallStartTime));
            paneloptContext.sysCallElapsedTimeMilliSeconds = sysCallElapsedTimeMilliSeconds;
            logger.info("sysCallElapsedTimeMilliSeconds [ms] = " + sysCallElapsedTimeMilliSeconds);

			// Get objective function values from file
            File panelOptoutfile1 = new File(scratchDir, "resp.vef");
            Vector<String> outputFileContents = GenericUtil.getFileContents(panelOptoutfile1);
            Double[] outputArray = new Double[outputFileContents.size()];
            int i = 0;
            for (String value : outputFileContents) {
                outputArray[i++] = new Double(value);
            }

            if (outputArray[1] > 1.003 || outputArray[3] > 1.0) {
                if (outputArray[1] > 1.003) {
                    logger.info("Buckling Constraints not satisfied");
                    outputArray[0] = outputArray[0] + penaltyFunction(outputArray[0], outputArray[1]);
                    logger.info("outputArray[0] ="+outputArray[0]);
                }
                else if (outputArray[3] > 1.0) {
                    logger.info("VM Constraints not satisfied");
                    outputArray[0] = outputArray[0] + penaltyFunction(outputArray[0], outputArray[3]);
                    logger.info("outputArray[0] ="+outputArray[0]);
                }
            }

            if (outputArray[4] > 0.0) {
                logger.info("Analysis failed");
                outputArray[0] = 20.0;
            } 

            paneloptContext.outputArray = outputArray;

        } catch (Throwable e) {
            StringBuilder reason = new StringBuilder();
            reason.append("*** error: WingoptProviderImpl caught exception = ").append(e.getClass().getName())
                    .append("\n\tscratchDir = ").append(scratchDir)
                    .append("\n\tscratchUrl = ").append(scratchUrl);
            context.reportException(reason.toString(), e, getProviderInfo("execute"));
            logger.log(Level.SEVERE, reason.toString(), e);
            return context;
        }

        long stopTime = System.currentTimeMillis();
        long serviceOpElapsedTimeMilliSeconds = stopTime - startTime;
        logger.info("serviceOpElapsedTimeMilliSeconds = " + serviceOpElapsedTimeMilliSeconds);

        paneloptContext.serviceOpElapsedTimeMilliSeconds = serviceOpElapsedTimeMilliSeconds;
        logger.info("deccremented threadsRunning = " +--threadsRunning);

        return paneloptContext;
    }

    private void setFileContents(File file, Object[] objA) throws IOException {

        Vector<String> vect = new Vector<String>();
        for (Object o : objA) {
            vect.add(o.toString());
        }
        GenericUtil.setFileContents(file, vect);
    }

    public static double penaltyFunction(double m, double b) {
        double penalty;
        double factor;
        factor = b - 1;
        logger.info("b = " + b);
        logger.info("factor = " + factor);
        penalty = Math.pow(Math.E, factor);
        logger.info("penalty = " + penalty);
        return penalty;
    }
}
