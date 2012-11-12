package org.lawrencebower.docgen.web.controller;

import org.apache.log4j.Logger;
import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.web.model.SessionData;
import org.lawrencebower.docgen.web_logic.business.controler_business.data_entry.DataEntryCB;
import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.lawrencebower.docgen.web_model.view.document_info.DocComponentView;
import org.lawrencebower.docgen.web_model.view.document_info.DocumentInfoView;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
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
@RequestMapping("/dataEntry")
public class DataEntryController {

    static Logger logger = Logger.getLogger(DataEntryController.class);

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

    @RequestMapping(value = "/prepareFields", method = RequestMethod.GET)
    public String prepareFields() {

        ContactView selectedCustomer = sessionData.getSelectedCustomer();
        List<ProductView> selectedProducts = sessionData.getSelectedProducts();
        ContactView selectedBusiness = sessionData.getSelectedBusiness();

        List<DocumentInfoView> documentInfos =
                business.getDocumentsForViewing(selectedCustomer,
                                                selectedProducts);

        sessionData.setDocuments(documentInfos);

        business.mapAutoMapFields(documentInfos,
                                  selectedCustomer,
                                  selectedBusiness);

        return "dataEntry";
    }

    @RequestMapping("/toggleAutomapped")
    public String toggleShowAutomappedFields(){
        boolean currentValue = sessionData.isShowAutoMappedFields();
        sessionData.setShowAutoMappedFields(!currentValue);

        return "dataEntry";
    }

    @RequestMapping(value = "/setFields", method = RequestMethod.POST)
    public String submitFields(WebRequest webRequest,
                               OutputStream outStream) {

        Map<String, String[]> parameterMap = webRequest.getParameterMap();

        List<DocumentInfoView> relevantDocs = sessionData.getDocuments();

        business.mapFieldValuesToComponents(parameterMap, relevantDocs);

        List<PDFDocument> pdFs = business.createPDFs(relevantDocs);

        sessionData.setGeneratedDocuments(pdFs);

        List<PDFDocument> generatedPDFs = sessionData.getPDFDocuments();

        List<File> allPdfFiles = business.writePDFsToFiles(generatedPDFs);

        File concatenatedFile = business.makeConcatenatedFile(allPdfFiles);

        business.writePDFsToStream(outStream, concatenatedFile);

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

    public List<DocComponentView> getDocComponentViews(){

        List<DocumentInfoView> relevantDocs = sessionData.getDocuments();

        boolean showAutoMappedFields = sessionData.isShowAutoMappedFields();

        return business.getComponentsForViewing(relevantDocs, showAutoMappedFields);
    }

}
