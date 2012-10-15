package org.lawrencebower.docgen.web.model;

import org.lawrencebower.docgen.web_model.view.customer.CustomerView;
import org.lawrencebower.docgen.web_model.view.document_info.DocumentInfoView;
import org.lawrencebower.docgen.web_model.view.product.ProductView;

import java.util.ArrayList;
import java.util.List;

public class SessionData {

    private CustomerView selectedCustomer;

    private List<ProductView> selectedProducts = new ArrayList<>();

    private List<DocumentInfoView> documents = new ArrayList<>();

    public void setSelectedCustomer(CustomerView selectedCustomer) {
        this.selectedCustomer = selectedCustomer;
    }

    public void setDocuments(List<DocumentInfoView> documents) {
        this.documents = documents;
    }

    public CustomerView getSelectedCustomer() {
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
}
