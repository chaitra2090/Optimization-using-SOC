/* The following method QnstoppProviderImpl.java is an analysis provider that provides
QNSTOPP as a service over the SORCER network, and is implemented in accordance with
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
package mil.afrl.mstc.open.qnstopp.provider;

import com.sun.jini.start.LifeCycle;
import mil.afrl.mstc.open.qnstopp.Qnstopp;
import mil.afrl.mstc.open.qnstopp.QnstoppContext;
import mil.afrl.mstc.open.core.provider.EngineeringProvider;
import sorcer.service.Context;
import sorcer.util.GenericUtil;

import java.io.File;
import java.net.URL;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.Vector;

/**
 * Implementation of the {@link mil.afrl.mstc.open.qnstopp.Qnstopp} interface
 *
 * @author MSTC Engineering Project Generator
 */
public class QnstoppProviderImpl extends EngineeringProvider implements Qnstopp {
    private final static Logger logger = Logger.getLogger(QnstoppProviderImpl.class.getName());

    /**
     * Default constructor, initializes the provider with the properties file loaded as a resource.
     *
     * @throws Exception If initialization fails.
     */
    public QnstoppProviderImpl() throws Exception {
        this("qnstopp/provider.properties");
    }

    /**
     * Create the provider using a properties file
     *
     * @param propFile The properties file, must not be {@code null}
     * @throws Exception If initialization fails.
     */
    public QnstoppProviderImpl(final String propFile) throws Exception {
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
    public QnstoppProviderImpl(final String[] args, final LifeCycle lifeCycle) throws Exception {
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
            // cast to domain-specific context
            //
            QnstoppContext qnstoppContext = (QnstoppContext) context;

            // get scratch dir
            //
            scratchDir = getScratchDir(context, "qnstopp_execute_");
            scratchUrl = getScratchURL(scratchDir);

            // download input file
            //
//            URL qnstoppInputUrl = (URL) qnstoppContext.getInput0();
//            File qnstoppInputFile = new File(scratchDir, "qnstopp.nml");
            URL qnstoppModelUrl = (URL) qnstoppContext.getInput1();
            File qnstoppModelFile = new File(scratchDir, "modelInfo.txt");
//            GenericUtil.download(qnstoppInputUrl, qnstoppInputFile);
            GenericUtil.download(qnstoppModelUrl, qnstoppModelFile);

            //Process p = Runtime.getRuntime().exec("./test1");

            // create script
            //
            String qnstoppExec = null;
            if (GenericUtil.isLinux()) qnstoppExec = getProperty("provider.exec.qnstopp.linux64").trim();
            if (GenericUtil.isMac()) qnstoppExec = getProperty("provider.exec.qnstopp.mac64").trim();

            File nativeHome = new File(qnstoppExec).getParentFile();
            Vector<String> script = new Vector<String>();
            script.add("#!/bin/bash");
            script.add("cd " + scratchDir.getAbsolutePath());
            script.add("cp " + new File(qnstoppExec).getAbsolutePath() + " .");
            script.add("cp " + nativeHome + "/qnstoppObjFunc.class .");
            script.add("cp " + nativeHome + "/qnstopp .");
//            script.add("cp " + nativeHome + "/resp.txt ."); // why is this here?
            script.add("echo \"going to run qnstops exe...\" >> prvLog.txt");
            script.add("./" + new File(qnstoppExec).getName());

            // run shell script
            //
            int exitStatus = runShellScript(scratchDir, script);
            //GenericUtil.runShellScript(new File(scratchDir, "vtscript.sh"), script, new File(scratchDir, "stdout.txt"), new File(scratchDir, "stderr.txt"));

            // add output to context
            //
            File qnstoppoutfile1 = new File(scratchDir, "QNSTOPPOutput.txt");
            URL qnstoppOutputFile = this.getScratchURL(qnstoppoutfile1);
            qnstoppContext.setOutput(qnstoppOutputFile);
            logger.info("QNSTOPPContext = " + qnstoppContext);

        } catch (Exception e) {
            StringBuilder reason = new StringBuilder();
            reason.append("*** error: qnstoppProviderImpl caught exception = ").append(e.getClass().getName())
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
