package org.lawrencebower.docgen.web_logic.business.controler_business.data_entry;

import org.apache.commons.io.IOUtils;
import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.utils.PDFConcatenator;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.AMComponentInfoImpl;
import org.lawrencebower.docgen.web_model.business_def.utils.PdfDirWriter;
import org.lawrencebower.docgen.web_model.business_def.utils.ViewUtils;
import org.lawrencebower.docgen.web_model.view.constants.ViewConstants;
import org.lawrencebower.docgen.web_model.view.contact.BusinessSelection;
import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.lawrencebower.docgen.web_model.view.contact.CustomerSelection;
import org.lawrencebower.docgen.web_model.view.document.DocumentInjectionInfo;
import org.lawrencebower.docgen.web_model.view.document.DocumentSet;
import org.lawrencebower.docgen.web_model.view.document.DocumentSetFactory;
import org.lawrencebower.docgen.web_model.view.document.DocumentView;
import org.lawrencebower.docgen.web_model.view.document.binding.DataEntryBindBean;
import org.lawrencebower.docgen.web_model.view.document.component.DocComponentView;
import org.lawrencebower.docgen.web_model.view.product.ProductSelection;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.lawrencebower.docgen.web_model.view.view_factory.ViewFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

public class DataEntryCB {

    @Autowired
    PDFConcatenator pdfConcatenator;
    @Autowired
    ViewUtils viewUtils;
    @Autowired(required = false)
    ViewFactory viewFactory;
    @Autowired
    private DocumentSetFactory documentSetFactory;
    @Autowired
    private PdfDirWriter pdfDirWriter;

    @Autowired
    @Qualifier("pdfOutputRoot")
    String fileRoot;

    public DocumentSet getDocumentsForViewing(CustomerSelection customerSelection,
                                              ProductSelection productSelection) {

        customerSelection.checkCustomerSet();
        productSelection.checkProductsSet();

        ContactView selectedCustomer = customerSelection.getSelectedCustomer();
        List<ProductView> products = productSelection.getProducts();

        return getRelevantDocuments(selectedCustomer, products);
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

    public void mapFieldValuesToComponents(DataEntryBindBean bindBean,
                                           DocumentSet documentSet) {

        documentSet.mapFieldValuesToComponents(bindBean);
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

        if (!pdfDocuments.isEmpty()) {
            File outputDir = pdfDirWriter.createPDFDir(fileRoot);
            writePDFsToFile(pdfDocuments, outputDir);
        }
    }

    private void writePDFsToFile(List<PDFDocument> pdfDocuments, File outputDir) {

        for (PDFDocument pdfDocument : pdfDocuments) {

            String docName = pdfDocument.getName();
            String nameExtension = pdfDocument.getNameExtension();
            String fileName = docName + nameExtension + ".pdf";
            String filePath = outputDir.getPath();

            File file = new File(filePath + File.separator + fileName);

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
                                     CustomerSelection customerSelection,
                                     BusinessSelection businessSelection) {

        AMComponentInfoImpl mappingInfo = createMappingInfo(customerSelection,
                                                            businessSelection);

        documentSet.mapAutomappedComponents(mappingInfo);

    }

    private AMComponentInfoImpl createMappingInfo(CustomerSelection customerSelection,
                                                  BusinessSelection businessSelection) {

        customerSelection.checkCustomerSet();
        businessSelection.checkBusinessSet();

        ContactView vendor = viewFactory.getVendor();

        ContactView selectedCustomer = customerSelection.getSelectedCustomer();
        ContactView selectedBusiness = businessSelection.getSelectedBusiness();

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
