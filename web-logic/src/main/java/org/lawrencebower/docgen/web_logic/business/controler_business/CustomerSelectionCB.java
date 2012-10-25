package org.lawrencebower.docgen.web_logic.business.controler_business;

import org.lawrencebower.docgen.web_logic.business.model_factory.ModelFactory;
import org.lawrencebower.docgen.web_model.view.business.BusinessView;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.List;

public class CustomerSelectionCB {

    @Autowired
    private ModelFactory modelFactory;

    public List<BusinessView> getCustomers() {
        return modelFactory.getCustomers();
    }

    public BusinessView getCustomer(String customerName) {
        return modelFactory.getCustomer(customerName);
    }

    public List<ProductView> getProducts() {
        return modelFactory.getProducts();
    }
}
