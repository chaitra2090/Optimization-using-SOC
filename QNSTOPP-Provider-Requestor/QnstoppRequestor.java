/* The following method QnstoppRequestor.java is a requestor that creates exertions
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
package mil.afrl.mstc.open.qnstopp.requestor;

import mil.afrl.mstc.open.qnstopp.Qnstopp;

import mil.afrl.mstc.open.data.DataService;
import mil.afrl.mstc.open.provider.ProviderStrategy;
import mil.afrl.mstc.open.requestor.EngineeringRequestor;
import mil.afrl.mstc.open.qnstopp.Qnstopp;
import mil.afrl.mstc.open.qnstopp.QnstoppContext;
import sorcer.core.requestor.SorcerRequestor;
import mil.afrl.mstc.open.task.TaskFactory;
import sorcer.core.context.ServiceContext;
import sorcer.core.exertion.NetTask;
import sorcer.core.signature.NetSignature;
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
 * <li>@{link mil.afrl.mstc.open.qnstopp.Qnstopp}</li>
 * </ul>
 *
 * @author MSTC Engineering Project Generator
 */
public class QnstoppRequestor extends SorcerRequestor{
    private static Logger logger = Logger.getLogger(QnstoppRequestor.class.getName());
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

//        File qnstoppInputFile = new File(engHome,"/optimization/qnstopp/qnstopp-req/data/qnstopp.nml");
        File modelInputFile = new File(engHome,"/optimization/qnstopp/qnstopp-req/data/modelInfo.txt");

        // edit modeInputFile to adjust model name for user
        Vector<String> modelInputFileContents = GenericUtil.getFileContents(modelInputFile);
        modelInputFileContents.setElementAt(Sorcer.getActualName(modelInputFileContents.elementAt(0).trim()), 0);
        GenericUtil.printVect(modelInputFileContents);

        File modelInputFileTemp = new File(tempDir, "modelInfo.txt");
        GenericUtil.setFileContents(modelInputFileTemp, modelInputFileContents);

//        URL qnstoppInputUrl = SorcerTester.copyFileToScratchAndGetUrl(qnstoppInputFile, dataDir);
        URL modelInputUrl = SorcerTester.copyFileToScratchAndGetUrl(modelInputFileTemp, dataDir);

        QnstoppContext context = new QnstoppContext("QNSTOPPContext");
//        context.setInput0(qnstoppInputUrl);
        context.setInput1(modelInputUrl);

        /* construct execute method and then Task */
        String providerName = Sorcer.getActualName("Engineering-Qnstopp");
        String serviceName = "execute";
        NetSignature methodEN = new NetSignature(serviceName,
                Qnstopp.class,
                providerName);

        NetTask qnstoppTask = new NetTask("run execute", "Task to run QNSTOPP", methodEN);

        /* Put the context into the task */
        qnstoppTask.setContext(context);
        logger.info("qnstoppTask = " + qnstoppTask);
        Exertion result = qnstoppTask.exert();
        logger.info("Returned Task after exert: " + result);
        logger.info("Context output = "+((QnstoppContext)result.getContext()).getOutput());

        URL qnstoppOutputUrl = (URL) ((QnstoppContext) result.getContext()).getOutput();
        Vector<String> qnstoppOutputFileContents = GenericUtil.getFileContents(qnstoppOutputUrl);
        GenericUtil.printVect(qnstoppOutputFileContents);

    }
}
