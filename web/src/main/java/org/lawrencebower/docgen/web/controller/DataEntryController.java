package org.lawrencebower.docgen.web.controller;

import org.apache.log4j.Logger;
import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.web.model.SessionData;
import org.lawrencebower.docgen.web_logic.business.controler_business.data_entry.DataEntryCB;
import org.lawrencebower.docgen.web_logic.view.contact.ContactView;
import org.lawrencebower.docgen.web_logic.view.document_info.DocComponentView;
import org.lawrencebower.docgen.web_logic.view.document_info.DocumentInfoView;
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

        setRelevantDocuments();

        mapAutoMappedFields();

        injectProductFields();

        processCalculatedFields();

        return "dataEntry";
    }

    private void setRelevantDocuments() {

        ContactView selectedCustomer = sessionData.getSelectedCustomer();
        List<ProductView> selectedProducts = sessionData.getSelectedProducts();

        List<DocumentInfoView> documentsForViewing =
                business.getDocumentsForViewing(selectedCustomer, selectedProducts);

        sessionData.setDocuments(documentsForViewing);
    }

    private void mapAutoMappedFields() {

        ContactView selectedBusiness = sessionData.getSelectedBusiness();
        ContactView selectedCustomer = sessionData.getSelectedCustomer();
        List<DocumentInfoView> documents = sessionData.getDocuments();

        business.mapAutoMapFields(documents,
                                  selectedCustomer,
                                  selectedBusiness);
    }

    private void injectProductFields() {
        List<DocumentInfoView> documents = sessionData.getDocuments();
        List<ProductView> selectedProducts = sessionData.getSelectedProducts();

        business.injectProductFields(documents, selectedProducts);
    }

    private void processCalculatedFields() {
        List<DocumentInfoView> documents = sessionData.getDocuments();
        business.processCalculatedFields(documents);
    }

    @RequestMapping("/toggleAutomapped")
    public String toggleShowAutomappedFields() {
        boolean currentValue = sessionData.isShowAutoMappedFields();
        sessionData.setShowAutoMappedFields(!currentValue);

        return "dataEntry";
    }

    @RequestMapping(value = "/setFields", method = RequestMethod.POST)
    public String submitFields(WebRequest webRequest,
                               OutputStream outStream) {

        mapFieldValuesToComponents(webRequest);

        List<File> allPdfFiles = generatePDFsAndWriteToFiles();

        concatenatePDFsAndWriteToResponse(outStream, allPdfFiles);

        return null;
    }

    private void concatenatePDFsAndWriteToResponse(OutputStream outStream, List<File> allPdfFiles) {
        File concatenatedFile = business.makeConcatenatedFile(allPdfFiles);
        business.writePDFsToStream(outStream, concatenatedFile);
    }

    private List<File> generatePDFsAndWriteToFiles() {

        List<DocumentInfoView> relevantDocs = sessionData.getDocuments();

        List<PDFDocument> generatedPDFs = business.createPDFs(relevantDocs);

        sessionData.setGeneratedPDFs(generatedPDFs);

        return business.writePDFsToFiles(generatedPDFs);
    }

    private void mapFieldValuesToComponents(WebRequest webRequest) {

        Map<String, String[]> parameterMap = webRequest.getParameterMap();

        writeParameterVals(parameterMap);

        List<DocumentInfoView> relevantDocs = sessionData.getDocuments();

        business.mapFieldValuesToComponents(parameterMap, relevantDocs);
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

    public List<DocComponentView> getDocComponentViews() {

        List<DocumentInfoView> relevantDocs = sessionData.getDocuments();

        boolean showAutoMappedFields = sessionData.isShowAutoMappedFields();

        return business.getComponentsForViewing(relevantDocs, showAutoMappedFields);
    }

}
