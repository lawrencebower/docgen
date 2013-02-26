package org.lawrencebower.docgen.web.controller.data_entry;

import org.apache.log4j.Logger;
import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.web.model.SessionData;
import org.lawrencebower.docgen.web_logic.business.controler_business.data_entry.DataEntryCB;
import org.lawrencebower.docgen.web_model.view.document.DocumentSet;
import org.lawrencebower.docgen.web_model.view.document.binding.DataEntryBindBean;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.InitBinder;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import java.io.File;
import java.io.OutputStream;
import java.util.List;

@Controller
@Scope("session")
public class SetFieldsController {

    static Logger logger = Logger.getLogger(SetFieldsController.class);

    private DataEntryCB business;
    private SessionData sessionData;

    @Autowired
    public void setBusiness(DataEntryCB business) {
        this.business = business;
    }

    @Autowired
    public void setSessionData(SessionData sessionData) {
        this.sessionData = sessionData;
    }

    @InitBinder
    public void initBinder(WebDataBinder binder) {
        binder.setAutoGrowNestedPaths(false);
    }

    @RequestMapping(value = "/dataEntry/setFields", method = RequestMethod.POST)
    public String submitFields(DataEntryBindBean bindBean,
                               OutputStream outStream) {

        mapFieldValuesToComponents(bindBean);

        DocumentSet injectedDocuments = injectDocuments();

        writeFilesToDiskAndResponse(injectedDocuments, outStream);

        return null;
    }

    private void mapFieldValuesToComponents(DataEntryBindBean bindBean) {

        DocumentSet documents = sessionData.getDocuments();

        business.mapFieldValuesToComponents(bindBean, documents);
    }

    private DocumentSet injectDocuments() {

        DocumentSet originalDocuments = sessionData.getDocuments();

        List<ProductView> products = sessionData.getSelectedProducts();

        return business.injectDocuments(originalDocuments, products);
    }

    private void writeFilesToDiskAndResponse(DocumentSet documents,
                                             OutputStream outputStream) {

        File outputDir = business.createOutputDir();

        List<PDFDocument> pdfDocuments = generatePDFsAndWriteToFiles(documents, outputDir);

        concatenatePDFsAndWriteToResponse(outputStream,
                                          pdfDocuments,
                                          outputDir);
    }

    private List<PDFDocument> generatePDFsAndWriteToFiles(DocumentSet documents,
                                                          File outputDir) {

        List<PDFDocument> generatedPDFs = business.createPDFs(documents);

        business.writePDFsToFiles(generatedPDFs, outputDir);

        return generatedPDFs;
    }

    private void concatenatePDFsAndWriteToResponse(OutputStream outStream,
                                                   List<PDFDocument> allPdfFiles,
                                                   File outputDir) {

        File concatenatedFile = business.makeConcatenatedFile(allPdfFiles, outputDir);

        business.writePDFsToStream(outStream, concatenatedFile);
    }

}
