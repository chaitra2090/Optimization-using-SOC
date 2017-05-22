/* The following method QnstopsRequestor.java is a requestor that creates exertions
and submits them to the grid in order to utilize services provided by the corresponding
analysis provider. */
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
package mil.afrl.mstc.open.qnstops.requestor;

import mil.afrl.mstc.open.qnstops.Qnstops;

import mil.afrl.mstc.open.data.DataService;
import mil.afrl.mstc.open.provider.ProviderStrategy;
import mil.afrl.mstc.open.requestor.EngineeringRequestor;
import mil.afrl.mstc.open.qnstops.Qnstops;
import mil.afrl.mstc.open.qnstops.QnstopsContext;
import sorcer.core.requestor.SorcerRequestor;
import mil.afrl.mstc.open.task.TaskFactory;
import sorcer.core.context.ServiceContext;
import sorcer.core.signature.NetSignature;
import sorcer.core.exertion.NetTask;
import sorcer.service.Context;
import sorcer.service.Exertion;
import sorcer.util.Sorcer;
import sorcer.util.GenericUtil;
import org.sorcer.core.requestor.SorcerTester;

import java.io.File;
import java.util.Properties;
import java.util.logging.Logger;
import java.net.URL;
import java.util.Vector;


/**
 * A requestor that uses the following providers:
 *
 * <ul>
 * <li>@{link mil.afrl.mstc.open.qnstops.Qnstops}</li>
 * </ul>
 *
 * @author MSTC Engineering Project Generator
 */
public class QnstopsRequestor extends SorcerRequestor {
    private static Logger logger = Logger.getLogger(QnstopsRequestor.class.getName());
    private File dataDir = new File(System.getProperty("data.dir"));
    private DataService dataService;
    private Properties properties;
    private EngineeringRequestor engineeringRequestor = new EngineeringRequestor();

    /**
     * Run the requestor
     *
     * @param args Arguments to process
     * @throws Exception If there are problems running the requestor
     */
    public void run(String... args) throws Exception {

        File engHome = new File(System.getProperty("eng.home"));
        File dataDir = new File(engHome, "data");
        File tempDir = new File(dataDir, "temp");

        File qnstopInputFile = new File(engHome,"/optimization/qnstops/qnstops-req/data/qnstops.nml");
        File modelInputFile25 = new File(engHome,"/optimization/qnstops/qnstops-req/data/modelInfo.txt");
        File modelInputFile13 = new File(engHome,"/optimization/qnstops/qnstops-req/data/modelInfo13.txt");

        // select 13 or 25 dv case here
        File modelInputFile = modelInputFile13;

        // edit modeInputFile to adjust model name for user
        Vector<String> modelInputFileContents = GenericUtil.getFileContents(modelInputFile);
        GenericUtil.printVect(modelInputFileContents);
        modelInputFileContents.setElementAt(Sorcer.getActualName(modelInputFileContents.elementAt(0).trim()), 0);
        GenericUtil.printVect(modelInputFileContents);

        File modelInputFileTemp = new File(tempDir, "modelInfo.txt");
        GenericUtil.setFileContents(modelInputFileTemp, modelInputFileContents);


        URL modelInputUrl = SorcerTester.copyFileToScratchAndGetUrl(modelInputFileTemp, dataDir);

        QnstopsContext context = new QnstopsContext("QNSTOPSContext");
        context.setInput1(modelInputUrl);

        /* construct execute method and then Task */
        String providerName = Sorcer.getActualName("Engineering-Qnstops");
        String serviceName = "execute";
        NetSignature methodEN = new NetSignature(serviceName,
                Qnstops.class,
                providerName);

        NetTask qnstopsTask = new NetTask("run execute", "Task to run QNSTOP", methodEN);

        /* Put the context into the task */
        qnstopsTask.setContext(context);
        logger.info("qnstopsTask = " + qnstopsTask);
        Exertion result = qnstopsTask.exert();
        logger.info("Returned Task after exert: " + result);
        logger.info("Context output = "+((QnstopsContext)result.getContext()).getOutput());

        URL qnstopsOutputUrl = (URL) ((QnstopsContext) result.getContext()).getOutput();
        Vector<String> qnstopOutputFileContents = GenericUtil.getFileContents(qnstopsOutputUrl);
        GenericUtil.printVect(qnstopOutputFileContents);

    }
}
