package org.lawrencebower.docgen.web_logic.business.model_factory;

import org.lawrencebower.docgen.web_model.view.business.BusinessView;
import org.lawrencebower.docgen.web_model.view.document_info.DocumentInfoView;
import org.lawrencebower.docgen.web_model.view.product.ProductView;

import java.util.ArrayList;
import java.util.List;

public class ModelFactoryFileImpl implements ModelFactory {

    @Override
    public List<BusinessView> getCustomers() {
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
    public BusinessView getCustomer(String customerName) {
        return null;
    }

    @Override
    public ProductView getProduct(String productId) {
        return null;
    }
}
