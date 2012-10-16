package org.lawrencebower.docgen.web.controller;

import org.apache.log4j.Logger;
import org.lawrencebower.docgen.web.model.SessionData;
import org.lawrencebower.docgen.web_logic.business.controler_business.DataEntryCB;
import org.lawrencebower.docgen.web_model.ViewConstants;
import org.lawrencebower.docgen.web_model.view.document_info.DocumentInfoView;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.context.request.WebRequest;

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

    @RequestMapping(method = RequestMethod.GET)
    public String prepareFields(Model model) {

        List<DocumentInfoView> documentInfos =
                business.getDocumentsForViewing(sessionData.getSelectedCustomer(),
                                                sessionData.getSelectedProducts());

        sessionData.setDocuments(documentInfos);

        return "dataEntry";
    }

    @RequestMapping(value = "/setFields", method = RequestMethod.POST)
    public String submitFields(WebRequest webRequest) {

        Map<String, String[]> parameterMap = webRequest.getParameterMap();

        for (String key : parameterMap.keySet()) {
            System.out.println("key = " + key);
            String[] values = parameterMap.get(key);
            for (String value : values) {
                System.out.println("value = " + value);
            }
        }

        business.mapFieldValuesToComponents(parameterMap,
                                            sessionData.getDocuments());

        business.createPDFs(sessionData.getDocuments());

        return "dataEntry";
    }

}
