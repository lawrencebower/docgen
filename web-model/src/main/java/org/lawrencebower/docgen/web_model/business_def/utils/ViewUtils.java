package org.lawrencebower.docgen.web_model.business_def.utils;

import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.lawrencebower.docgen.web_model.view.product.ProductView;

import java.util.List;

public interface ViewUtils {

    void checkBusinessSet(ContactView selectedBusiness);//todo make collection object

    void checkProductsSet(List<ProductView> selectedProducts);//todo make collection object

    void checkCustomerSet(ContactView selectedCustomer);//todo make collection object

    String toHTMLString(String string);
}
