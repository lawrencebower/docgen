package org.lawrencebower.docgen.web_logic.business.controler_business;

import org.lawrencebower.docgen.web_logic.business.model_factory.ModelFactory;
import org.lawrencebower.docgen.web_model.view.business.ContactView;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.List;

public class CustomerSelectionCB {

    @Autowired
    private ModelFactory modelFactory;

    public List<ContactView> getCustomers() {
        return modelFactory.getCustomers();
    }

    public ContactView getCustomer(String customerName) {
        return modelFactory.getCustomer(customerName);
    }

    public ContactView getBusinessByCustomerName(String customerName) {
        return modelFactory.getBusinessByCustomerName(customerName);
    }

    public List<ProductView> getProducts() {
        return modelFactory.getProducts();
    }
}
