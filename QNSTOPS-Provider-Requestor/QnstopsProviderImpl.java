/* The following method QnstopsProviderImpl.java is an analysis provider that provides
QNSTOPS as a service over the SORCER network, and is implemented in accordance with
principles of exertion-oriented programming (EOP) as described in Chapter 3.2. */
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
package mil.afrl.mstc.open.qnstops.provider;

import com.sun.jini.start.LifeCycle;
import mil.afrl.mstc.open.qnstops.Qnstops;
import mil.afrl.mstc.open.qnstops.QnstopsContext;
import mil.afrl.mstc.open.core.provider.EngineeringProvider;
import sorcer.service.Context;
import sorcer.util.GenericUtil;

import java.io.File;
import java.net.URL;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Implementation of the {@link mil.afrl.mstc.open.qnstops.Qnstops} interface
 *
 * @author MSTC Engineering Project Generator
 */
public class QnstopsProviderImpl extends EngineeringProvider implements Qnstops {
    private final static Logger logger = Logger.getLogger(QnstopsProviderImpl.class.getName());

    /**
     * Default constructor, initializes the provider with the properties file loaded as a resource.
     *
     * @throws Exception If initialization fails.
     */
    public QnstopsProviderImpl() throws Exception {
        this("qnstops/provider.properties");
    }

    /**
     * Create the provider using a properties file
     *
     * @param propFile The properties file, must not be {@code null}
     * @throws Exception If initialization fails.
     */
    public QnstopsProviderImpl(final String propFile) throws Exception {
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
    public QnstopsProviderImpl(final String[] args, final LifeCycle lifeCycle) throws Exception {
        super(args, lifeCycle);
        initialize();
    }

    /*
     * Provides provider specific initialization and environment checking
     */
    private void initialize() {

    }

    public Context execute(final Context context) {
        long startTime = System.currentTimeMillis();
        logger.info("\n************* Running execute *****************");
        File scratchDir = null;
        URL scratchUrl = null;
        String serviceIdString = doThreadMonitor(null);

		/*
		 * You need to fill in the details here for provider implementation.
		 */
        try {
            // Cast to domain-specific context
            QnstopsContext qnstopsContext = (QnstopsContext) context;

            // Get scratch dir
            scratchDir = getScratchDir(context, "qnstops_execute_");
            scratchUrl = getScratchURL(scratchDir);

            // Download input file for QNSTOPS and model input file
            URL qnstopModelUrl = (URL) qnstopsContext.getInput1();
            File qnstopModelFile = new File(scratchDir, "modelInfo.txt");
            GenericUtil.download(qnstopModelUrl, qnstopModelFile);

            // Get system properties
            String qnstopExec = null;
            if (GenericUtil.isLinux()) qnstopExec = getProperty("provider.exec.qnstops.linux64").trim();
            if (GenericUtil.isMac()) qnstopExec = getProperty("provider.exec.qnstops.mac64").trim();

			// Create script to run QNSTOPS
            File nativeHome = new File(qnstopExec).getParentFile();
            Vector<String> script = new Vector<String>();
            script.add("#!/bin/bash");
            script.add("cd " + scratchDir.getAbsolutePath());
            script.add("cp " + new File(qnstopExec).getAbsolutePath() + " .");
            script.add("cp " + nativeHome + "/qnstopsObjFunc.class .");
            script.add("cp " + nativeHome + "/qnstops .");
            script.add("echo \"going to run qnstops exe...\" >> prvLog.txt");
            script.add("./" + new File(qnstopExec).getName() + " &> qnstopStdOutAndError.txt");

            // Run shell script
            File scriptFile = new File(scratchDir, "script.sh");
            GenericUtil.setFileContents(scriptFile, script);
            scriptFile.setExecutable(true);
            Process p = Runtime.getRuntime().exec(new String[]{"sh", "-c", scriptFile.getAbsolutePath()});
            p.waitFor();

            // Add output to context
            File qnstopoutfile1 = new File(scratchDir, "QNSTOPSOutput.txt");
            URL qnstopOutputFile = this.getScratchURL(qnstopoutfile1);
            qnstopsContext.setOutput(qnstopOutputFile);
            logger.info("QNSTOPSContext = " + qnstopsContext);

        } catch (Exception e) {
            StringBuilder reason = new StringBuilder();
            reason.append("*** error: qnstopsProviderImpl caught exception = ").append(e.getClass().getName())
                    .append("\n\tscratchDir = ").append(scratchDir)
                    .append("\n\tscratchUrl = ").append(scratchUrl);

            context.reportException(reason.toString(), e, getProviderInfo("execute"));
            logger.log(Level.SEVERE, reason.toString(), e);
            return context;
        }
        logger.info("\n\n *************** Returning from service execute ********************* \n Output context = "+context
                    +"\n********************************************************************\n\n");
        return context;
    }

}
