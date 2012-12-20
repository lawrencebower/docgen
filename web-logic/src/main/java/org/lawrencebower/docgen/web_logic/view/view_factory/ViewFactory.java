package org.lawrencebower.docgen.web_logic.view.view_factory;

import org.lawrencebower.docgen.web_logic.view.contact.ContactView;
import org.lawrencebower.docgen.web_logic.view.document.DocumentView;
import org.lawrencebower.docgen.web_logic.view.product.ProductView;

import java.util.ArrayList;
import java.util.List;

public interface ViewFactory {

    List<ContactView> getCustomers();

    List<DocumentView> getAllDocuments();

    DocumentView createDocument(String documentName);

    List<ProductView> getProducts();

    ContactView getCustomer(String customerName);

    ProductView getProduct(String productId);

    ContactView getVendor();

    ContactView getBusinessByCustomerName(String customerName);

    ArrayList<ContactView> getBusinesses();

    List<DocumentView> getDocumentsForCustomerAndProduct(ContactView customer, ProductView product);
}
