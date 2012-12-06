package org.lawrencebower.docgen.web_logic.business.utils;

import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_logic.view.contact.ContactView;
import org.lawrencebower.docgen.web_logic.view.document.DocumentView;
import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;
import org.lawrencebower.docgen.web_logic.view.product.ProductView;

import java.util.ArrayList;
import java.util.List;

public class ViewUtils {

    public static final String NO_CUSTOMER_SELECTED = "No customer selected?!";
    public static final String NO_PRODUCTS_SELECTED = "No products selected?!";
    public static final String NO_BUSINESS_SELECTED = "No business selected?!";
    public static final String NO_DOCUMENTS_SELECTED = "No documents selected?!";

    public List<DocComponentView> getAllComponentViewsFromDocs(List<DocumentView> documents) {

        List<DocComponentView> results = new ArrayList<>();

        for (DocumentView documentView : documents) {
            List<DocComponentView> docComponentViews = documentView.getComponentViews();
            results.addAll(docComponentViews);
        }

        return results;
    }

    public void checkBusinessSet(ContactView selectedBusiness) {
        if (selectedBusiness == null) {
            throw new DocGenException(NO_BUSINESS_SELECTED);
        }
    }

    public void checkProductsSet(List<ProductView> selectedProducts) {
        if ((selectedProducts == null) || selectedProducts.isEmpty()) {
            throw new DocGenException(NO_PRODUCTS_SELECTED);
        }
    }

    public void checkDocumentsSet(List<DocumentView> selectedDocuments) {
        if ((selectedDocuments == null) || selectedDocuments.isEmpty()) {
            throw new DocGenException(NO_DOCUMENTS_SELECTED);
        }
    }

    public void checkCustomerSet(ContactView selectedCustomer) {
        if (selectedCustomer == null) {
            throw new DocGenException(NO_CUSTOMER_SELECTED);
        }
    }

    public String toHTMLString(String string) {
        return string.replaceAll("\n","<br/>");
    }
}
