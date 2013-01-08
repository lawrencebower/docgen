package org.lawrencebower.docgen.web_logic.business.utils;

import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_model.business_def.utils.ViewUtils;
import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.lawrencebower.docgen.web_model.view.product.ProductView;

import java.util.List;

public class ViewUtilsImpl implements ViewUtils {

    public static final String NO_CUSTOMER_SELECTED = "No customer selected?!";
    public static final String NO_PRODUCTS_SELECTED = "No products selected?!";
    public static final String NO_BUSINESS_SELECTED = "No business selected?!";

    @Override
    public void checkBusinessSet(ContactView selectedBusiness) {
        if (selectedBusiness == null) {
            throw new DocGenException(NO_BUSINESS_SELECTED);
        }
    }

    @Override
    public void checkProductsSet(List<ProductView> selectedProducts) {
        if ((selectedProducts == null) || selectedProducts.isEmpty()) {
            throw new DocGenException(NO_PRODUCTS_SELECTED);
        }
    }

    @Override
    public void checkCustomerSet(ContactView selectedCustomer) {
        if (selectedCustomer == null) {
            throw new DocGenException(NO_CUSTOMER_SELECTED);
        }
    }

    @Override
    public String toHTMLString(String string) {
        return string.replaceAll("\n","<br/>");
    }
}
