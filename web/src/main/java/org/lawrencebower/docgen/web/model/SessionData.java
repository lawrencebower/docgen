package org.lawrencebower.docgen.web.model;

import org.lawrencebower.docgen.web_logic.view.contact.ContactView;
import org.lawrencebower.docgen.web_logic.view.document.DocumentSet;
import org.lawrencebower.docgen.web_logic.view.document.DocumentView;
import org.lawrencebower.docgen.web_logic.view.product.Product;
import org.lawrencebower.docgen.web_logic.view.product.ProductSelection;
import org.lawrencebower.docgen.web_logic.view.product.ProductView;

import java.util.List;

public class SessionData {

    private ContactView selectedCustomer;

    private ContactView selectedBusiness;

    private ProductSelection selectedProducts = new ProductSelection();

    private DocumentSet documents;

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

    public void setDocuments(DocumentSet documents) {
        this.documents = documents;
    }

    public List<ProductView> getSelectedProducts() {
        return selectedProducts.getProducts();
    }

    public List<DocumentView> getDocumentsAsList() {
        return documents.getDocumentsAsList();
    }

    public DocumentSet getDocuments() {
        return documents;
    }

    public void addSelectedProduct(Product selectedProduct) {
        selectedProducts.addProduct(selectedProduct);
    }

    public boolean isHasProducts() {
        return selectedProducts.hasProducts();
    }

    public boolean isShowAutoMappedFields() {
        return showAutoMappedFields;
    }

    public void setShowAutoMappedFields(boolean showAutoMappedFields) {
        this.showAutoMappedFields = showAutoMappedFields;
    }
}
