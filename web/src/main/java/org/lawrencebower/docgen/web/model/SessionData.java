package org.lawrencebower.docgen.web.model;

import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.web_logic.view.contact.ContactView;
import org.lawrencebower.docgen.web_logic.view.document_info.DocumentInfoView;
import org.lawrencebower.docgen.web_logic.view.product.Product;
import org.lawrencebower.docgen.web_logic.view.product.ProductSelection;
import org.lawrencebower.docgen.web_logic.view.product.ProductView;

import java.util.ArrayList;
import java.util.List;

public class SessionData {

    private ContactView selectedCustomer;

    private ContactView selectedBusiness;

    private ProductSelection selectedProducts = new ProductSelection();

    private List<DocumentInfoView> documents = new ArrayList<>();

    private List<PDFDocument> generatedDocuments;

    private boolean showAutoMappedFields = false;

    public void setSelectedCustomer(ContactView selectedCustomer) {
        this.selectedCustomer = selectedCustomer;
    }

    public ContactView getSelectedCustomer() {
        return selectedCustomer;
    }

    public ContactView getSelectedBusiness() {
        return selectedBusiness;
    }

    public void setSelectedBusiness(ContactView selectedBusiness) {
        this.selectedBusiness = selectedBusiness;
    }

    public void setDocuments(List<DocumentInfoView> documents) {
        this.documents = documents;
    }

    public List<ProductView> getSelectedProducts() {
        return selectedProducts.getProducts();
    }

    public List<DocumentInfoView> getDocuments() {
        return documents;
    }

    public void addSelectedProduct(Product selectedProduct) {
        selectedProducts.addProduct(selectedProduct);
    }

    public void setGeneratedPDFs(List<PDFDocument> generatedDocuments) {
        this.generatedDocuments = generatedDocuments;
    }

    public List<PDFDocument> getGeneratedPDFs() {
        return generatedDocuments;
    }

    public boolean isShowAutoMappedFields() {
        return showAutoMappedFields;
    }

    public void setShowAutoMappedFields(boolean showAutoMappedFields) {
        this.showAutoMappedFields = showAutoMappedFields;
    }
}
