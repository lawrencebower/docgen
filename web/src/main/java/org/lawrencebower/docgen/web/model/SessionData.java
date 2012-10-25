package org.lawrencebower.docgen.web.model;

import org.lawrencebower.docgen.core.generator.model.PDFDocument;
import org.lawrencebower.docgen.web_model.view.business.ContactView;
import org.lawrencebower.docgen.web_model.view.document_info.DocumentInfoView;
import org.lawrencebower.docgen.web_model.view.product.ProductView;

import java.util.ArrayList;
import java.util.List;

public class SessionData {

    private ContactView selectedCustomer;

    private List<ProductView> selectedProducts = new ArrayList<>();

    private List<DocumentInfoView> documents = new ArrayList<>();

    private List<PDFDocument> generatedDocuments;

    private boolean showAutoMappedFields = false;

    public void setSelectedCustomer(ContactView selectedCustomer) {
        this.selectedCustomer = selectedCustomer;
    }

    public void setDocuments(List<DocumentInfoView> documents) {
        this.documents = documents;
    }

    public ContactView getSelectedCustomer() {
        return selectedCustomer;
    }

    public List<ProductView> getSelectedProducts() {
        return selectedProducts;
    }

    public List<DocumentInfoView> getDocuments() {
        return documents;
    }

    public void addSelectedProduct(ProductView selectedProduct) {
       selectedProducts.add(selectedProduct);
    }

    public void setGeneratedDocuments(List<PDFDocument> generatedDocuments) {
        this.generatedDocuments = generatedDocuments;
    }

    public List<PDFDocument> getPDFDocuments() {
        return generatedDocuments;
    }

    public boolean isShowAutoMappedFields() {
        return showAutoMappedFields;
    }

    public void setShowAutoMappedFields(boolean showAutoMappedFields) {
        this.showAutoMappedFields = showAutoMappedFields;
    }
}
