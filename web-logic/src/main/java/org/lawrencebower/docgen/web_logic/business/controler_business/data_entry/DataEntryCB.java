package org.lawrencebower.docgen.web_logic.business.controler_business.data_entry;

import org.apache.commons.io.IOUtils;
import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.utils.PDFConcatenator;
import org.lawrencebower.docgen.web_logic.business.mapping.AutoMappedComponentInfo;
import org.lawrencebower.docgen.web_logic.business.mapping.CustomerProduct_Document_Mappings;
import org.lawrencebower.docgen.web_logic.business.mapping.FieldMapper;
import org.lawrencebower.docgen.web_logic.business.model_factory.ModelFactory;
import org.lawrencebower.docgen.web_logic.business.utils.ViewUtils;
import org.lawrencebower.docgen.web_logic.view.constants.ViewConstants;
import org.lawrencebower.docgen.web_logic.view.contact.Contact;
import org.lawrencebower.docgen.web_logic.view.contact.ContactView;
import org.lawrencebower.docgen.web_logic.view.document_info.DocumentInfoView;
import org.lawrencebower.docgen.web_logic.view.document_info.component.DocComponentView;
import org.lawrencebower.docgen.web_logic.view.product.Product;
import org.lawrencebower.docgen.web_logic.view.product.ProductView;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.*;

public class DataEntryCB {

    @Autowired
    PDFConcatenator pdfConcatenator;
    @Autowired
    ViewUtils viewUtils;
    @Autowired
    ModelFactory modelFactory;
    @Autowired
    private CustomerProduct_Document_Mappings customerProductMappings;
    @Autowired
    FieldMapper fieldMapper;
    @Autowired
    private ViewableComponentFilter viewableComponentFilter;

    @Autowired
    @Qualifier("pdfOutputRoot")
    String fileRoot;

    public List<DocumentInfoView> getDocumentsForViewing(ContactView selectedCustomer,
                                                         List<ProductView> selectedProducts) {

        viewUtils.checkCustomerSet(selectedCustomer);
        viewUtils.checkProductsSet(selectedProducts);

        return getRelevantDocuments(selectedCustomer, selectedProducts);
    }

    private ArrayList<DocumentInfoView> getRelevantDocuments(ContactView selectedCustomer,
                                                             List<ProductView> selectedProducts) {

        Contact customer = selectedCustomer.getContact();
        Set<DocumentInfoView> docInfos = new LinkedHashSet<>();//preserve order - for tests

        for (ProductView selectedProduct : selectedProducts) {
            Product product = selectedProduct.getProduct();
            List<DocumentInfoView> docInfo = customerProductMappings.getDocInfosForCustomerAndProduct(customer, product);
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
            PDFDocument pdfDocument = document.generatePDF();
            String documentName = document.getName();
            pdfDocument.setName(documentName);
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

    public void mapAutoMapComponents(List<DocumentInfoView> documentInfos,
                                     ContactView selectedCustomer,
                                     ContactView selectedBusiness) {

        AutoMappedComponentInfo mappingInfo = createMappingInfo(documentInfos,
                                                                selectedCustomer,
                                                                selectedBusiness);

        List<DocComponentView> components = viewUtils.getAllComponentViewsFromDocs(documentInfos);

        for (DocComponentView component : components) {
            component.mapComponentValue(mappingInfo);
        }
    }

    private AutoMappedComponentInfo createMappingInfo(List<DocumentInfoView> documentInfos,
                                                      ContactView selectedCustomer,
                                                      ContactView selectedBusiness) {

        viewUtils.checkCustomerSet(selectedCustomer);
        viewUtils.checkBusinessSet(selectedBusiness);
        viewUtils.checkDocumentsSet(documentInfos);

        Contact customerContact = selectedCustomer.getContact();

        ContactView vendor = modelFactory.getVendor();
        Contact vendorContact = vendor.getContact();

        Contact businessContact = selectedBusiness.getContact();

        return new AutoMappedComponentInfo(customerContact,
                                           vendorContact,
                                           businessContact);
    }

    public List<DocComponentView> getComponentsForViewing(List<DocumentInfoView> documents,
                                                          boolean showAutoMappedFields) {
        List<DocComponentView> results;

        if (showAutoMappedFields) {
            results = viewableComponentFilter.getComponents(documents);
        } else {
            results = viewableComponentFilter.getNonAutoMappedComponents(documents);
        }

        return results;
    }

    public void injectProductFields(List<DocumentInfoView> documents,
                                    List<ProductView> selectedProducts) {
        List<DocComponentView> componentViews = viewUtils.getAllComponentViewsFromDocs(documents);
        for (DocComponentView componentView : componentViews) {
            componentView.injectProducts(selectedProducts);
        }
    }

    public void processCalculatedFields(List<DocumentInfoView> documents) {
        List<DocComponentView> componentViews = viewUtils.getAllComponentViewsFromDocs(documents);
        for (DocComponentView componentView : componentViews) {
            componentView.calculateValue(componentViews);
        }

    }

//    SETTERS FOR UNIT TESTS

    protected void setPdfConcatenator(PDFConcatenator pdfConcatenator) {
        this.pdfConcatenator = pdfConcatenator;
    }

    protected void setCustomerProductMappings(CustomerProduct_Document_Mappings customerProductMappings) {
        this.customerProductMappings = customerProductMappings;
    }

}
