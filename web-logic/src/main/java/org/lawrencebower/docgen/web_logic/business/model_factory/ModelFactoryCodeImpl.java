package org.lawrencebower.docgen.web_logic.business.model_factory;

import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_logic.business.mapping.CustomerProduct_Document_Mappings;
import org.lawrencebower.docgen.web_model.view.contact.Contact;
import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.lawrencebower.docgen.web_model.view.document_info.DocumentInfoView;
import org.lawrencebower.docgen.web_model.view.product.Product;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.springframework.beans.factory.annotation.Autowired;
import usecase.CommercialInvoice;
import usecase.DeliveryNote;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

public class ModelFactoryCodeImpl implements ModelFactory {

    @Autowired
    private CommercialInvoice commercialInvoice;
    @Autowired
    private DeliveryNote deliveryNote;
    @Autowired
    private CustomerProduct_Document_Mappings customerProductDocMappings;

    private LinkedHashMap<String, ContactView> customers = new LinkedHashMap<>();

    private LinkedHashMap<String, ContactView> businesses = new LinkedHashMap<>();

    private LinkedHashMap<String, ProductView> products = new LinkedHashMap<>();

    private LinkedHashMap<String, DocumentInfoView> documents = new LinkedHashMap<>();

    private ContactView vendor;
    private ContactView customer1;
    private ContactView customer2;
    private ProductView product1;
    private ProductView product2;
    private DocumentInfoView commercialInvoiceView;
    private DocumentInfoView deliveryNoteView;

    public static final String CUSTOMER_ID_1 = "Contact 1";
    public static final String CUSTOMER_ID_2 = "Contact 2";
    public static final String PRODUCT_ID_1 = "id1";
    public static final String PRODUCT_ID_2 = "id2";
    public static final String DOC_1_NAME = CommercialInvoice.INVOICE_NAME;
    public static final String DOC_2_NAME = DeliveryNote.DELIVERY_NOTE_NAME;
    public static final String AUTO_MAPPED_EXAMPLE_FIELD = "soldToName:";
    public static final String EXAMPLE_FIELD = "Date:";

    public void init() {
        initVendor();
        initCustomers();
        initProducts();
        initDocuments();
        initCustomerProductDocumentMappings();
    }

    private void initVendor() {
        vendor = new ContactView(new Contact("Billy Bob's Widgets",
                                             "Billy Bob",
                                             "36 Billy Bob Street\nColchester\nEssex",
                                             "534546454",
                                             "UK",
                                             "tax id",
                                             "sales@ziath.com"));
    }

    private void initDocuments() {

        commercialInvoiceView = commercialInvoice.getDocInfoView();

        deliveryNoteView = deliveryNote.getDocInfo();

        documents.put(commercialInvoiceView.getName(), commercialInvoiceView);
        documents.put(deliveryNoteView.getName(), deliveryNoteView);
    }

    private void initProducts() {

        product1 = new ProductView(new Product(PRODUCT_ID_1, "name 1"));
        product2 = new ProductView(new Product(PRODUCT_ID_2, "name 2"));

        products.put(product1.getId(), product1);
        products.put(product2.getId(), product2);
    }

    private void initCustomers() {

        customer1 = new ContactView(new Contact(CUSTOMER_ID_1,
                                                "David Davidson",
                                                "Just round the corner",
                                                "198293893839",
                                                "UK"));

        customer2 = new ContactView(new Contact(CUSTOMER_ID_2,
                                                "Billy Bob Bobson",
                                                "miles away",
                                                "38783478347",
                                                "CHINA"));

        customers.put(customer1.getName(), customer1);
        customers.put(customer2.getName(), customer2);

        businesses.put(customer1.getName(), customer2);
        businesses.put(customer2.getName(), customer1);
    }

    private void initCustomerProductDocumentMappings() {

        //Contact 1
        customerProductDocMappings.addDocument(customer1.getContact(),
                                               product1.getProduct(),
                                               commercialInvoiceView);

        customerProductDocMappings.addDocument(customer1.getContact(),
                                               product2.getProduct(),
                                               commercialInvoiceView);

        customerProductDocMappings.addDocument(customer1.getContact(),
                                               product2.getProduct(),
                                               deliveryNoteView);

        //Contact 2
        customerProductDocMappings.addDocument(customer2.getContact(),
                                               product1.getProduct(),
                                               commercialInvoiceView);

        customerProductDocMappings.addDocument(customer2.getContact(),
                                               product2.getProduct(),
                                               commercialInvoiceView);

        customerProductDocMappings.addDocument(customer2.getContact(),
                                               product2.getProduct(),
                                               deliveryNoteView);

    }

    @Override
    public List<ContactView> getCustomers() {
        return new ArrayList<>(customers.values());
    }

    @Override
    public ArrayList<ContactView> getBusinesses() {
        return new ArrayList<>(businesses.values());
    }

    @Override
    public List<ProductView> getProducts() {
        return new ArrayList<>(products.values());
    }

    public List<DocumentInfoView> getDocuments() {
        return new ArrayList<>(documents.values());
    }

    public ContactView getCustomer(String customerName) {
        if (!customers.containsKey(customerName)) {
            throw new DocGenException(String.format("Contact %s not found?!", customerName));
        }
        return customers.get(customerName);
    }

    @Override
    public ContactView getBusinessByCustomerName(String customerName) {
        if (!businesses.containsKey(customerName)) {
            throw new DocGenException(String.format("Business %s not found?!", customerName));
        }
        return businesses.get(customerName);
    }

    @Override
    public ProductView getProduct(String productId) {
        if (!products.containsKey(productId)) {
            throw new DocGenException(String.format("product '%s' not found?!", productId));
        }
        return products.get(productId);
    }

    @Override
    public ContactView getVendor() {
        return vendor;
    }
}
