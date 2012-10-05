package org.lawrencebower.docgen.web_logic.business.xml_mapper;

import org.lawrencebower.docgen.web_logic.view.customer.CustomerView;
import org.lawrencebower.docgen.web_logic.view.document_info.DocumentInfoView;
import org.lawrencebower.docgen.web_logic.view.product.ProductView;

import java.util.ArrayList;
import java.util.List;

public class ModelFactoryFileImpl implements ModelFactory {

    @Override
    public List<CustomerView> getCustomers() {
        return new ArrayList<>();
    }

    @Override
    public List<DocumentInfoView> getDocuments() {
        return null;
    }

    @Override
    public List<ProductView> getProducts() {
        return null;
    }

    @Override
    public CustomerView getCustomer(String customerName) {
        return null;
    }

    @Override
    public ProductView getProduct(String productId) {
        return null;
    }
}
