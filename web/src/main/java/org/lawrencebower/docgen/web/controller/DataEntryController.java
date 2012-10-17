package org.lawrencebower.docgen.web.controller;

import org.apache.log4j.Logger;
import org.lawrencebower.docgen.core.generator.model.PDFDocument;
import org.lawrencebower.docgen.web.model.SessionData;
import org.lawrencebower.docgen.web_logic.business.controler_business.DataEntryCB;
import org.lawrencebower.docgen.web_model.view.document_info.DocumentInfoView;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.context.request.WebRequest;

import java.io.OutputStream;
import java.util.List;
import java.util.Map;

@Controller
@Scope("session")
@RequestMapping("/dataEntry")
public class DataEntryController {

    static Logger logger = Logger.getLogger(DataEntryController.class);

    @Autowired
    private DataEntryCB business;
    @Autowired
    private SessionData sessionData;

    public DataEntryController() {
    }

    @RequestMapping(value = "/prepareFields", method = RequestMethod.GET)
    public String prepareFields(Model model) {

        List<DocumentInfoView> documentInfos =
                business.getDocumentsForViewing(sessionData.getSelectedCustomer(),
                                                sessionData.getSelectedProducts());

        sessionData.setDocuments(documentInfos);

        return "dataEntry";
    }

    @RequestMapping(value = "/addTableRow", method = RequestMethod.GET)
    public String addTableRow() {
        System.out.println("DataEntryController.addTableRow");
        return "dataEntry";
    }

    @RequestMapping(value = "/setFields", method = RequestMethod.POST)
    public String submitFields(WebRequest webRequest,
                               OutputStream outStream) {

        Map<String, String[]> parameterMap = webRequest.getParameterMap();

        writeParameterVals(parameterMap);

        business.mapFieldValuesToComponents(parameterMap,
                                            sessionData.getDocuments());

        if (parameterMap.containsKey("partial")) {
            return "redirect:/dataEntry/addTableRow";
        }

        List<PDFDocument> pdFs = business.createPDFs(sessionData.getDocuments());

        sessionData.setGeneratedDocuments(pdFs);

        business.writePDFsToFile(sessionData.getPDFDocuments());

        business.writePDFsToStream(outStream, sessionData.getPDFDocuments());

        return null;
    }

    private void writeParameterVals(Map<String, String[]> parameterMap) {
        for (String key : parameterMap.keySet()) {
            System.out.println("key = " + key);
            String[] values = parameterMap.get(key);
            for (String value : values) {
                System.out.println("value = " + value);
            }
        }
    }

}
