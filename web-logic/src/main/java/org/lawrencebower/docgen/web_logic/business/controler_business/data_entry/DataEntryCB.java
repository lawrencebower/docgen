package org.lawrencebower.docgen.web_logic.business.controler_business.data_entry;

import org.apache.commons.io.IOUtils;
import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.utils.PDFConcatenator;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.AMComponentInfoImpl;
import org.lawrencebower.docgen.web_logic.view.constants.ViewConstants;
import org.lawrencebower.docgen.web_logic.view.contact.ContactView;
import org.lawrencebower.docgen.web_logic.view.document.*;
import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;
import org.lawrencebower.docgen.web_logic.view.product.ProductView;
import org.lawrencebower.docgen.web_logic.view.view_factory.ViewFactory;
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
    ViewFactory viewFactory;
    @Autowired
    private DocumentSetFactory documentSetFactory;

    @Autowired
    @Qualifier("pdfOutputRoot")
    String fileRoot;

    public DocumentSet getDocumentsForViewing(ContactView selectedCustomer,
                                              List<ProductView> selectedProducts) {

        viewUtils.checkCustomerSet(selectedCustomer);
        viewUtils.checkProductsSet(selectedProducts);

        return getRelevantDocuments(selectedCustomer, selectedProducts);
    }

    private DocumentSet getRelevantDocuments(ContactView selectedCustomer,
                                             List<ProductView> selectedProducts) {

        /**
         * LinkedHashSet is used to ensure no duplicate Documents are included and the order
         * of insertion is preserved (helpful in tests)
         */
        Set<DocumentView> documents = new LinkedHashSet<>();

        for (ProductView selectedProduct : selectedProducts) {
            List<DocumentView> documentViews =
                    viewFactory.getDocumentsForCustomerAndProduct(selectedCustomer, selectedProduct);
            documents.addAll(documentViews);
        }

        return documentSetFactory.createDocumentInfoSet(documents);
    }

    public void mapFieldValuesToComponents(Map<String, String[]> parameterMap,
                                           DocumentSet documentSet) {

        documentSet.mapFieldValuesToComponents(parameterMap);
    }

    public List<PDFDocument> createPDFs(DocumentSet documentSet) {
        return documentSet.createPDFs();
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

    public void writePDFsToFiles(List<PDFDocument> pdfDocuments) {

        for (PDFDocument pdfDocument : pdfDocuments) {

            String docName = pdfDocument.getName();
            String nameExtension = pdfDocument.getNameExtension();
            String fileName = fileRoot + docName + nameExtension + ".pdf";
            File file = new File(fileName);

            pdfDocument.setFile(file);
            pdfDocument.writeToFile(file);
        }
    }

    public File makeConcatenatedFile(List<PDFDocument> allFiles) {
        String pathName = fileRoot + ViewConstants.CONCATENATED_FILE_NAME;
        File concatenatedFile = new File(pathName);
        pdfConcatenator.concatenatePDFs(allFiles, concatenatedFile);

        return concatenatedFile;
    }

    public void mapAutoMapComponents(DocumentSet documentSet,
                                     ContactView selectedCustomer,
                                     ContactView selectedBusiness) {

        AMComponentInfoImpl mappingInfo = createMappingInfo(selectedCustomer,
                                                        selectedBusiness);

        documentSet.mapAutomappedComponents(mappingInfo);

    }

    private AMComponentInfoImpl createMappingInfo(ContactView selectedCustomer,
                                              ContactView selectedBusiness) {

        viewUtils.checkCustomerSet(selectedCustomer);
        viewUtils.checkBusinessSet(selectedBusiness);

        ContactView vendor = viewFactory.getVendor();

        return new AMComponentInfoImpl(selectedCustomer,
                                   vendor,
                                   selectedBusiness);
    }

    public List<DocComponentView> getComponentsForViewing(DocumentSet documentSet,
                                                          boolean showAutoMappedFields) {

        return documentSet.getComponentsForViewing(showAutoMappedFields);

    }

    public void injectProductFields(DocumentSet documentSet,
                                    List<ProductView> selectedProducts) {

        documentSet.injectProductFields(selectedProducts);
    }

    public void processCalculatedFields(DocumentSet documentSet) {
        documentSet.processCalculatedFields();
    }

    public DocumentSet injectDocuments(DocumentSet originalDocuments,
                                       List<ProductView> products) {

        List<DocumentInjectionInfo> injectionInfos = makeDocumentInjectionInfo(products);

        return originalDocuments.injectDocuments(injectionInfos);
    }

    private List<DocumentInjectionInfo> makeDocumentInjectionInfo(List<ProductView> products) {

        List<DocumentInjectionInfo> injectionInfos = new ArrayList<>();

        for (ProductView product : products) {
            DocumentInjectionInfo info = new DocumentInjectionInfo(product);
            injectionInfos.add(info);
        }

        return injectionInfos;
    }

//    SETTERS FOR UNIT TESTS

    protected void setPdfConcatenator(PDFConcatenator pdfConcatenator) {
        this.pdfConcatenator = pdfConcatenator;
    }

    protected void setViewFactory(ViewFactory viewFactory) {
        this.viewFactory = viewFactory;
    }

}
