package org.lawrencebower.docgen.web_model.business_def.utils;

import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.lawrencebower.docgen.web_model.view.document.DocumentView;
import org.lawrencebower.docgen.web_model.view.document.component.DocComponentView;
import org.lawrencebower.docgen.web_model.view.product.ProductView;

import java.util.List;

public interface ViewUtils {
    List<DocComponentView> getAllComponentViewsFromDocs(List<DocumentView> documents);

    void checkBusinessSet(ContactView selectedBusiness);

    void checkProductsSet(List<ProductView> selectedProducts);

    void checkDocumentsSet(List<DocumentView> selectedDocuments);

    void checkCustomerSet(ContactView selectedCustomer);

    String toHTMLString(String string);
}
