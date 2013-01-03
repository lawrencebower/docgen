package org.lawrencebower.docgen.web_logic.view.view_factory;

import org.lawrencebower.docgen.web_model.view.contact.Contact;
import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.lawrencebower.docgen.web_model.view.contact.ContactViewFactory;
import org.lawrencebower.docgen.web_model.view.document.DocumentView;
import org.lawrencebower.docgen.web_model.view.document.DocumentViewImpl;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.lawrencebower.docgen.web_model.view.view_factory.ViewFactory;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class ViewFactoryTestImpl implements ViewFactory {

    @Autowired
    private ContactViewFactory contactViewFactory;

    @Override
    public List<ContactView> getCustomers() {
        return new ArrayList<>();
    }

    @Override
    public List<DocumentView> getAllDocuments() {
        return null;
    }

    @Override
    public DocumentViewImpl createDocument(String documentName) {
        DocumentViewImpl mock = mock(DocumentViewImpl.class);
        when(mock.getName()).thenReturn(documentName);
        return mock;
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
    public ProductView getProduct(String productId) {
        return null;
    }

    @Override
    public ContactView getVendor() {
        Contact contact = new Contact("Billy Bob's Widgets",
                                 "Billy Bob",
                                 "36 Billy Bob Street\nColchester\nEssex",
                                 "534546454",
                                 "UK",
                                 "tax id",
                                 "sales@acme.com");

        ContactView vendor = contactViewFactory.createContactView(contact);

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

    @Override
    public List<DocumentView> getDocumentsForCustomerAndProduct(ContactView customer, ProductView product) {
        return null;
    }
}
