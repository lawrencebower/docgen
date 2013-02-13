package org.lawrencebower.docgen.web.model;

import org.lawrencebower.docgen.web_model.view.contact.BusinessSelection;
import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.lawrencebower.docgen.web_model.view.contact.CustomerSelection;
import org.lawrencebower.docgen.web_model.view.document.DocumentSet;
import org.lawrencebower.docgen.web_model.view.document.DocumentView;
import org.lawrencebower.docgen.web_model.view.product.ProductSelection;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.List;

public class SessionData {

    private CustomerSelection customerSelection = new CustomerSelection();

    private BusinessSelection businessSelection = new BusinessSelection();

    private ProductSelection productSelection;

    private DocumentSet documents;

    private boolean showAutoMappedFields;

    private SessionData() {//force spring creation
    }

    public void setCustomerSelection(ContactView customerSelection) {
        this.customerSelection.selectCustomer(customerSelection);
    }

    public ContactView getSelectedCustomer() {
        return customerSelection.getSelectedCustomer();
    }

    public CustomerSelection getCustomerSelection() {
        return customerSelection;
    }

    public ContactView getSelectedBusiness() {
        return businessSelection.getSelectedBusiness();
    }

    public BusinessSelection getBusinessSelection() {
        return businessSelection;
    }

    public void setSelectedBusiness(ContactView selectedBusiness) {
        businessSelection.selectBusiness(selectedBusiness);
    }

    public void setDocuments(DocumentSet documents) {
        this.documents = documents;
    }

    public List<ProductView> getSelectedProducts() {
        return productSelection.getProducts();
    }

    public ProductSelection getProductSelection() {
        return productSelection;
    }

    public void clearSelectedProducts() {
        productSelection.clear();
    }

    public List<DocumentView> getDocumentsAsList() {
        return documents.getDocumentsAsList();
    }

    public DocumentSet getDocuments() {
        return documents;
    }

    public void addSelectedProduct(ProductView selectedProduct) {
        productSelection.addProduct(selectedProduct);
    }

    public boolean isHasProducts() {
        return productSelection.hasProducts();
    }

    public boolean isShowAutoMappedFields() {
        return showAutoMappedFields;
    }

    public void setShowAutoMappedFields(boolean showAutoMappedFields) {
        this.showAutoMappedFields = showAutoMappedFields;
    }

    @Autowired
    public void setProductSelection(ProductSelection selectedProducts) {
        this.productSelection = selectedProducts;
    }

    public void clear() {
        customerSelection.clear();
        businessSelection.clear();
        productSelection.clear();
        if (documents != null) {
            documents.clear();
        }
    }
}
