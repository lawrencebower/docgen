package org.lawrencebower.docgen.web_model.view.view_factory;

import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.lawrencebower.docgen.web_model.view.document.DocumentView;
import org.lawrencebower.docgen.web_model.view.document.DocumentViewImpl;
import org.lawrencebower.docgen.web_model.view.product.ProductView;

import java.util.ArrayList;
import java.util.List;

public interface ViewFactory {

    List<ContactView> getCustomers();

    List<DocumentView> getAllDocuments();

    DocumentViewImpl createDocument(String documentName);

    List<ProductView> getProducts();

    ContactView getCustomer(String customerName);

    ProductView getProduct(String productId);

    ContactView getVendor();

    ContactView getBusinessByCustomerName(String customerName);

    ArrayList<ContactView> getBusinesses();

    List<DocumentView> getDocumentsForCustomerAndProduct(ContactView customer, ProductView product);
}
