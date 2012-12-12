package org.lawrencebower.docgen.web.controller.data_entry;

import org.apache.log4j.Logger;
import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.web.model.SessionData;
import org.lawrencebower.docgen.web_logic.business.controler_business.data_entry.DataEntryCB;
import org.lawrencebower.docgen.web_logic.view.document.DocumentSet;
import org.lawrencebower.docgen.web_logic.view.product.ProductView;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.context.request.WebRequest;

import java.io.File;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;

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

    @RequestMapping(value = "/dataEntry/setFields", method = RequestMethod.POST)
    public String submitFields(WebRequest webRequest,
                               OutputStream outStream) {

        mapFieldValuesToComponents(webRequest);

        DocumentSet injectedDocuments = injectDocuments();

        List<PDFDocument> pdfFiles = generatePDFsAndWriteToFiles(injectedDocuments);

        concatenatePDFsAndWriteToResponse(outStream, pdfFiles);

        return null;
    }

    private void mapFieldValuesToComponents(WebRequest webRequest) {

        Map<String, String[]> parameterMap = webRequest.getParameterMap();

//        writeParameterVals(parameterMap);

        DocumentSet documents = sessionData.getDocuments();

        business.mapFieldValuesToComponents(parameterMap, documents);
    }

    /*
        private void writeParameterVals(Map<String, String[]> parameterMap) {
            for (String key : parameterMap.keySet()) {
                System.out.println("key = " + key);
                String[] values = parameterMap.get(key);
                for (String value : values) {
                    System.out.println("value = " + value);
                }
            }
        }
    */

    private DocumentSet injectDocuments() {

        DocumentSet originalDocuments = sessionData.getDocuments();

        List<ProductView> products = sessionData.getSelectedProducts();

        return business.injectDocuments(originalDocuments, products);
    }

    private List<PDFDocument> generatePDFsAndWriteToFiles(DocumentSet documents) {

        List<PDFDocument> generatedPDFs = business.createPDFs(documents);

        business.writePDFsToFiles(generatedPDFs);

        return generatedPDFs;
    }

    private void concatenatePDFsAndWriteToResponse(OutputStream outStream, List<PDFDocument> allPdfFiles) {
        File concatenatedFile = business.makeConcatenatedFile(allPdfFiles);
        business.writePDFsToStream(outStream, concatenatedFile);
    }

}
