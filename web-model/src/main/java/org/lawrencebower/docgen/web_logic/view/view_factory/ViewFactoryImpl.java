package org.lawrencebower.docgen.web_logic.view.view_factory;

import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_logic.view.contact.ContactView;
import org.lawrencebower.docgen.web_logic.view.document.DocumentView;
import org.lawrencebower.docgen.web_logic.view.product.ProductView;
import org.lawrencebower.docgen.web_logic.view.view_factory.factory.CustomerFactory;
import org.lawrencebower.docgen.web_logic.view.view_factory.factory.DocumentFactory;
import org.lawrencebower.docgen.web_logic.view.view_factory.factory.ProductFactory;
import org.lawrencebower.docgen.web_logic.view.view_factory.factory.VendorFactory;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

public class ViewFactoryImpl implements ViewFactory {

    private CustomerProduct_Document_Mappings customerProductDocMappings;

    private Map<String, ContactView> customers;

    private Map<String, ContactView> businesses;

    private Map<String, ProductView> products;

    private ContactView vendor;

    private VendorFactory vendorFactory;
    private ProductFactory productFactory;
    private CustomerFactory customerFactory;
    private CustomerProductMappingFactory customerProductFactory;
    private DocumentFactory documentFactory;

    public void init() {
        vendor = vendorFactory.getVendor();
        products = productFactory.getProducts();
        customers = customerFactory.getCustomers();
        businesses = customerFactory.getBusinesses();
        customerProductDocMappings = customerProductFactory.getMappingInfo();
    }

    @Override
    public List<ContactView> getCustomers() {
        Collection<ContactView> contactViews = customers.values();
        return new ArrayList<>(contactViews);
    }

    @Override
    public ArrayList<ContactView> getBusinesses() {
        Collection<ContactView> contactViews = businesses.values();
        return new ArrayList<>(contactViews);
    }

    @Override
    public List<DocumentView> getDocumentsForCustomerAndProduct(ContactView customer, ProductView product) {

        List<String> documentNames =
                customerProductDocMappings.getDocumentsForCustomerAndProduct(customer, product);

        List<DocumentView> results = new ArrayList<>();

        for (String documentName : documentNames) {
            DocumentView documentView = createDocument(documentName);
            results.add(documentView);
        }

        return results;
    }

    @Override
    public List<ProductView> getProducts() {

        List<ProductView> results = new ArrayList<>();
        for (ProductView product : products.values()) {
            results.add(product);
        }

        return results;
    }

    public List<DocumentView> getAllDocuments() {
        return documentFactory.getAllDocuments();
    }

    @Override
    public DocumentView createDocument(String documentName) {
        return documentFactory.createDocument(documentName);
    }

    public ContactView getCustomer(String customerName) {
        if (!customers.containsKey(customerName)) {
            String message = String.format("Contact %s not found?!", customerName);
            throw new DocGenException(message);
        }
        return customers.get(customerName);
    }

    @Override
    public ContactView getBusinessByCustomerName(String customerName) {
        if (!businesses.containsKey(customerName)) {
            String message = String.format("Business %s not found?!", customerName);
            throw new DocGenException(message);
        }
        return businesses.get(customerName);
    }

    @Override
    public ProductView getProduct(String productId) {
        if (!products.containsKey(productId)) {
            String message = String.format("product '%s' not found?!", productId);
            throw new DocGenException(message);
        }
        return products.get(productId);
    }

    @Override
    public ContactView getVendor() {
        return vendor;
    }

    public void setCustomerProductFactory(CustomerProductMappingFactory customerProductFactory) {
        this.customerProductFactory = customerProductFactory;
    }

    public void setDocumentFactory(DocumentFactory documentFactory) {
        this.documentFactory = documentFactory;
    }

    public void setVendorFactory(VendorFactory vendorFactory) {
        this.vendorFactory = vendorFactory;
    }

    public void setProductFactory(ProductFactory productFactory) {
        this.productFactory = productFactory;
    }

    public void setCustomerFactory(CustomerFactory customerFactory) {
        this.customerFactory = customerFactory;
    }
}
