package org.lawrencebower.docgen.web.model;

import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.lawrencebower.docgen.web_model.view.document.DocumentSet;
import org.lawrencebower.docgen.web_model.view.document.DocumentView;
import org.lawrencebower.docgen.web_model.view.product.ProductSelection;
import org.lawrencebower.docgen.web_model.view.product.ProductView;

import java.util.List;

public class SessionData {

    private ContactView selectedCustomer;

    private ContactView selectedBusiness;

    private ProductSelection selectedProducts = new ProductSelection();

    private DocumentSet documents;

    private boolean showAutoMappedFields;

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

    public ProductSelection getProductSelection(){
        return selectedProducts;
    }

    public void clearSelectedProducts() {
        selectedProducts.clear();
    }

    public List<DocumentView> getDocumentsAsList() {
        return documents.getDocumentsAsList();
    }

    public DocumentSet getDocuments() {
        return documents;
    }

    public void addSelectedProduct(ProductView selectedProduct) {
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
