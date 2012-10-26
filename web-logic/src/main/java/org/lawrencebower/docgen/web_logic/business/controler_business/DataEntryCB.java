package org.lawrencebower.docgen.web_logic.business.controler_business;

import org.apache.commons.io.IOUtils;
import org.lawrencebower.docgen.core.document.DocumentInfo;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.model.PDFDocument;
import org.lawrencebower.docgen.core.generator.utils.PDFConcatenator;
import org.lawrencebower.docgen.web_logic.business.mapping.AutoMappedFieldMapper;
import org.lawrencebower.docgen.web_logic.business.mapping.CustomerProduct_Document_Mappings;
import org.lawrencebower.docgen.web_logic.business.mapping.FieldMapper;
import org.lawrencebower.docgen.web_logic.business.model_factory.ModelFactory;
import org.lawrencebower.docgen.web_logic.business.utils.ViewUtils;
import org.lawrencebower.docgen.web_model.view.contact.Contact;
import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.lawrencebower.docgen.web_model.view.constants.ViewConstants;
import org.lawrencebower.docgen.web_model.view.document_info.DocComponentView;
import org.lawrencebower.docgen.web_model.view.document_info.DocumentInfoView;
import org.lawrencebower.docgen.web_model.view.product.Product;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.*;

public class DataEntryCB {

    @Autowired
    FieldMapper fieldMapper;
    @Autowired
    PDFConcatenator pdfConcatenator;
    @Autowired
    AutoMappedFieldMapper reservedFieldMapper;
    @Autowired
    ViewUtils viewUtils;
    @Autowired
    ModelFactory modelFactory;

    @Autowired
    @Qualifier("pdfOutputRoot")
    String fileRoot;

    @Autowired
    private CustomerProduct_Document_Mappings mappings;

    public List<DocumentInfoView> getDocumentsForViewing(ContactView selectedBusiness,
                                                         List<ProductView> selectedProducts) {

        ArrayList<DocumentInfoView> relevantDocuments =
                getRelevantDocuments(selectedBusiness, selectedProducts);

        return relevantDocuments;
    }

    private ArrayList<DocumentInfoView> getRelevantDocuments(ContactView selectedBusiness,
                                                             List<ProductView> selectedProducts) {

        Contact business = selectedBusiness.getContact();
        Set<DocumentInfoView> docInfos = new HashSet<>();

        for (ProductView selectedProduct : selectedProducts) {
            Product product = selectedProduct.getproduct();
            List<DocumentInfoView> docInfo = mappings.getDocInfosForCustomerAndProduct(business, product);
            docInfos.addAll(docInfo);
        }

        return new ArrayList<>(docInfos);
    }

    public void mapFieldValuesToComponents(Map<String, String[]> parameterMap,
                                           List<DocumentInfoView> documents) {

        fieldMapper.mapFieldValuesToComponents(parameterMap, documents);
    }

    public List<PDFDocument> createPDFs(List<DocumentInfoView> documents) {

        List<PDFDocument> results = new ArrayList<>();

        for (DocumentInfoView document : documents) {
            DocumentInfo documentInfo = document.createDocumentInfo();
            PDFDocument pdfDocument = documentInfo.generatePDF();
            pdfDocument.setName(document.getName());
            results.add(pdfDocument);
        }

        return results;
    }

    public void writePDFsToStream(OutputStream outStream, File file) {
        try {
            FileInputStream inStream = new FileInputStream(file);
            byte[] bytes = IOUtils.toByteArray(inStream);
            outStream.write(bytes);
        } catch (IOException e) {
            throw new DocGenException(e);
        }
    }

    public List<File> writePDFsToFiles(List<PDFDocument> pdfDocuments) {

        List<File> allFiles = new ArrayList<>();

        for (PDFDocument pdfDocument : pdfDocuments) {
            String docName = pdfDocument.getName();
            String fileName = fileRoot + docName + ".pdf";
            File file = new File(fileName);

            pdfDocument.writeToFile(file);

            allFiles.add(file);
        }

        return allFiles;
    }

    public File makeConcatenatedFile(List<File> allFiles) {
        String pathName = fileRoot + ViewConstants.CONCATENATED_FILE_NAME;
        File concatenatedFile = new File(pathName);
        pdfConcatenator.concatenatePDFs(allFiles, concatenatedFile);

        return concatenatedFile;
    }

    public void mapAutoMapFields(List<DocumentInfoView> documentInfos,
                                 ContactView selectedCustomer,
                                 ContactView selectedBusiness) {

        Contact customerContact = selectedCustomer.getContact();

        ContactView vendor = modelFactory.getVendor();
        Contact vendorContact = vendor.getContact();

        Contact businessContact = selectedBusiness.getContact();

        reservedFieldMapper.mapFields(documentInfos,
                                      customerContact,
                                      vendorContact,
                                      businessContact);
    }

    public List<DocComponentView> getComponentsForViewing(List<DocumentInfoView> documents,
                                                          boolean showAutoMappedFields) {

        List<DocComponentView> results = filterAutomapped(documents, showAutoMappedFields);

        results = filterDuplicatedFields(results);

        return results;
    }

    private List<DocComponentView> filterAutomapped(List<DocumentInfoView> documents,
                                                    boolean showAutoMappedFields) {

        List<DocComponentView> results = new ArrayList<>();

        List<DocComponentView> componentViews = viewUtils.getAllComponentViewsFromDocs(documents);

        for (DocComponentView docComponentView : componentViews) {
            if (isComponentViewable(showAutoMappedFields, docComponentView)) {
                results.add(docComponentView);
            }
        }
        return results;
    }

    private boolean isComponentViewable(boolean showAutoMappedFields, DocComponentView docComponentView) {

        boolean componentViewable = true;

        if (showAutoMappedFields == false && docComponentView.isAutoMappedField()) {
            componentViewable = false;
        }

        return componentViewable;
    }

    /**
     * Adds the DocComponentViews to a Set and returns a unique list in the order they were added.
     * There will be one DocComponent with each unique component name in the set. The Set evaluates
     * the equality of the DocComponentViews based on the DocComponentView names.
     */
    private ArrayList<DocComponentView> filterDuplicatedFields(List<DocComponentView> documents) {

        LinkedHashSet<DocComponentView> filteredViews = new LinkedHashSet<>();
        for (DocComponentView document : documents) {
            filteredViews.add(document);
        }

        return new ArrayList<>(filteredViews);
    }

}
