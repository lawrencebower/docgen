package org.lawrencebower.docgen.web.controller;

import org.apache.log4j.Logger;
import org.lawrencebower.docgen.web.model.SessionData;
import org.lawrencebower.docgen.web_logic.business.controler_business.DataEntryCB;
import org.lawrencebower.docgen.web_model.view.document_info.DocumentInfoView;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;

import java.util.List;

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

    @RequestMapping({"/prepareFields"})
    public String prepareFields(Model model) {

        List<DocumentInfoView> documentInfos =
                business.getDocumentsForViewing(sessionData.getSelectedCustomer(),
                                                sessionData.getSelectedProducts());

        sessionData.setDocuments(documentInfos);

        return "dataEntry";
    }

    @RequestMapping({"/submitFields"})
    public String submitFields(SessionData sessionDataParam) {

        return "dataEntry";
    }

}
