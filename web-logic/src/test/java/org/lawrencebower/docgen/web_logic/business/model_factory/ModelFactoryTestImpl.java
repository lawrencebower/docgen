package org.lawrencebower.docgen.web_logic.business.model_factory;

import org.lawrencebower.docgen.web_logic.view.contact.Contact;
import org.lawrencebower.docgen.web_logic.view.contact.ContactView;
import org.lawrencebower.docgen.web_logic.view.document_info.DocumentInfoView;
import org.lawrencebower.docgen.web_logic.view.product.Product;
import org.lawrencebower.docgen.web_logic.view.product.ProductView;

import java.util.ArrayList;
import java.util.List;

public class ModelFactoryTestImpl implements ModelFactory {

    @Override
    public List<ContactView> getCustomers() {
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
    public ContactView getCustomer(String customerName) {
        return null;
    }

    @Override
    public Product getProduct(String productId) {
        return null;
    }

    @Override
    public ContactView getVendor() {
        ContactView vendor = new ContactView(new Contact("Billy Bob's Widgets",
                                                         "Billy Bob",
                                                         "36 Billy Bob Street\nColchester\nEssex",
                                                         "534546454",
                                                         "UK",
                                                         "tax id",
                                                         "sales@acme.com"));

        return vendor;
    }

    @Override
    public ContactView getBusinessByCustomerName(String customerName) {
        return null;
    }

    @Override
    public ArrayList<ContactView> getBusinesses() {
        return null;
    }
}
