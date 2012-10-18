package org.lawrencebower.docgen.web_logic.business.controler_business;

import org.apache.commons.io.IOUtils;
import org.lawrencebower.docgen.core.document.DocumentInfo;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.model.PDFDocument;
import org.lawrencebower.docgen.core.generator.utils.PDFConcatenator;
import org.lawrencebower.docgen.web_logic.business.mapping.CustomerProduct_Document_Mappings;
import org.lawrencebower.docgen.web_logic.business.mapping.FieldMapper;
import org.lawrencebower.docgen.web_model.ViewConstants;
import org.lawrencebower.docgen.web_model.view.customer.Customer;
import org.lawrencebower.docgen.web_model.view.customer.CustomerView;
import org.lawrencebower.docgen.web_model.view.document_info.DocumentInfoView;
import org.lawrencebower.docgen.web_model.view.product.Product;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

import java.io.*;
import java.util.*;

public class DataEntryCB {

    @Autowired
    FieldMapper fieldMapper;
    @Autowired
    PDFConcatenator pdfConcatenator;
    @Autowired
    @Qualifier("pdfOutputRoot")
    String fileRoot;

    @Autowired
    private CustomerProduct_Document_Mappings mappings;

    public List<DocumentInfoView> getDocumentsForViewing(CustomerView selectedCustomer,
                                                         List<ProductView> selectedProducts) {

        ArrayList<DocumentInfoView> relevantDocuments =
                getRelevantDocuments(selectedCustomer, selectedProducts);

        filterDuplicatedFields(relevantDocuments);

        return relevantDocuments;
    }

    private void filterDuplicatedFields(List<DocumentInfoView> documents) {
        //todo add filterer
    }

    private ArrayList<DocumentInfoView> getRelevantDocuments(CustomerView selectedCustomer,
                                                             List<ProductView> selectedProducts) {

        Customer customer = selectedCustomer.getCustomer();
        Set<DocumentInfoView> docInfos = new HashSet<>();

        for (ProductView selectedProduct : selectedProducts) {
            Product product = selectedProduct.getproduct();
            List<DocumentInfoView> docInfo = mappings.getDocInfosForCustomerAndProduct(customer, product);
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
}
