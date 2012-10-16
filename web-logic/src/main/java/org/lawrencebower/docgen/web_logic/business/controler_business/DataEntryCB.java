package org.lawrencebower.docgen.web_logic.business.controler_business;

import org.lawrencebower.docgen.core.document.DocumentInfo;
import org.lawrencebower.docgen.core.generator.model.PDFDocument;
import org.lawrencebower.docgen.web_logic.business.mapping.CustomerProduct_Document_Mappings;
import org.lawrencebower.docgen.web_logic.business.mapping.FieldMapper;
import org.lawrencebower.docgen.web_logic.business.mapping.ModelToViewMapper;
import org.lawrencebower.docgen.web_model.view.customer.Customer;
import org.lawrencebower.docgen.web_model.view.customer.CustomerView;
import org.lawrencebower.docgen.web_model.view.document_info.DocumentInfoView;
import org.lawrencebower.docgen.web_model.view.product.Product;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.springframework.beans.factory.annotation.Autowired;

import java.io.File;
import java.util.*;

public class DataEntryCB {

    @Autowired
    FieldMapper fieldMapper;

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

    public void createPDFs(List<DocumentInfoView> documents) {

        for (DocumentInfoView document : documents) {
            DocumentInfo documentInfo = document.createDocumentInfo();
            PDFDocument pdfDocument = documentInfo.generatePDF();

            String docName = documentInfo.getName();
            String fileName = "C:\\code\\output\\" + docName + ".pdf";
            File file = new File(fileName);

            pdfDocument.writeToFile(file);
        }

    }
}