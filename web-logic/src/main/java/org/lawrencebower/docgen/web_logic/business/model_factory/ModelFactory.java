package org.lawrencebower.docgen.web_logic.business.model_factory;

import org.lawrencebower.docgen.web_logic.view.contact.ContactView;
import org.lawrencebower.docgen.web_logic.view.document.DocumentView;
import org.lawrencebower.docgen.web_logic.view.product.Product;
import org.lawrencebower.docgen.web_logic.view.product.ProductView;

import java.util.ArrayList;
import java.util.List;

public interface ModelFactory {

    List<ContactView> getCustomers();

    List<DocumentView> getDocuments();

    List<ProductView> getProducts();

    ContactView getCustomer(String customerName);

    Product getProduct(String productId);

    ContactView getVendor();

    ContactView getBusinessByCustomerName(String customerName);

    ArrayList<ContactView> getBusinesses();
}
