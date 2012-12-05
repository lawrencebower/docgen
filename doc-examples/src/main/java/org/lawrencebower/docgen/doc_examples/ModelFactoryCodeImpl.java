package org.lawrencebower.docgen.doc_examples;

import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_logic.business.mapping.CustomerProduct_Document_Mappings;
import org.lawrencebower.docgen.web_logic.business.model_factory.ModelFactory;
import org.lawrencebower.docgen.web_logic.view.contact.Contact;
import org.lawrencebower.docgen.web_logic.view.contact.ContactView;
import org.lawrencebower.docgen.web_logic.view.document.DocumentView;
import org.lawrencebower.docgen.web_logic.view.product.Product;
import org.lawrencebower.docgen.web_logic.view.product.ProductView;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;

public abstract class ModelFactoryCodeImpl implements ModelFactory {

    @Autowired
    private CustomerProduct_Document_Mappings customerProductDocMappings;

    private LinkedHashMap<String, ContactView> customers = new LinkedHashMap<>();

    private LinkedHashMap<String, ContactView> businesses = new LinkedHashMap<>();

    private LinkedHashMap<String, Product> products = new LinkedHashMap<>();

    private ContactView vendor;
    private ContactView customer1;
    private ContactView customer2;
    private Product product1;
    private Product product2;

    public static final String CUSTOMER_ID_1 = "Billy Bob's products";
    public static final String CUSTOMER_ID_2 = "David's widgets";
    public static final String PRODUCT_ID_1 = "W1";
    public static final String PRODUCT_ID_2 = "W2";
    public static final String DOC_1_NAME = CommercialInvoice.INVOICE_NAME;
    public static final String DOC_2_NAME = DeliveryNote.DELIVERY_NOTE_NAME;
    public static final String AUTO_MAPPED_EXAMPLE_FIELD = "soldToName:";
    public static final String EXAMPLE_FIELD = "Date:";

    public void init() {
        initVendor();
        initCustomers();
        initProducts();
        initCustomerProductDocumentMappings();
    }

    private void initVendor() {
        vendor = new ContactView(new Contact("Billy Bob's Widgets",
                                             "Billy Bob",
                                             "36 Billy Bob Street\nColchester\nEssex",
                                             "534546454",
                                             "UK",
                                             "123456",
                                             "sales@acme.com"));
    }

    private void initProducts() {

        product1 = new Product(PRODUCT_ID_1,
                               "Super Widget 1",
                               "100.25",
                               "UK",
                               "Optical widget (no laser, plastic)");

        product2 = new Product(PRODUCT_ID_2,
                               "Mega Widget 2",
                               "200",
                               "UK",
                               "Laser widget (contains laser)",
                               "84562574");

        products.put(product1.getProductId(), product1);
        products.put(product2.getProductId(), product2);

    }

    private void initCustomers() {

        customer1 = new ContactView(new Contact(CUSTOMER_ID_1,
                                                "Billy Bob Bobson",
                                                "Just round the corner",
                                                "198293893839",
                                                "UK"));

        customer2 = new ContactView(new Contact(CUSTOMER_ID_2,
                                                "David Davidson",
                                                "miles away",
                                                "38783478347",
                                                "USA"));

        customers.put(customer1.getName(), customer1);
        customers.put(customer2.getName(), customer2);

        businesses.put(customer1.getName(), customer2);
        businesses.put(customer2.getName(), customer1);
    }

    private void initCustomerProductDocumentMappings() {

        //Contact 1
        customerProductDocMappings.addDocument(customer1.getContact(),
                                               product1,
                                               CommercialInvoice.INVOICE_NAME);

        customerProductDocMappings.addDocument(customer1.getContact(),
                                               product1,
                                               DeliveryNote.DELIVERY_NOTE_NAME);

        customerProductDocMappings.addDocument(customer1.getContact(),
                                               product2,
                                               CommercialInvoice.INVOICE_NAME);

        customerProductDocMappings.addDocument(customer1.getContact(),
                                               product2,
                                               DeliveryNote.DELIVERY_NOTE_NAME);

        //Contact 2
        customerProductDocMappings.addDocument(customer2.getContact(),
                                               product1,
                                               CommercialInvoice.INVOICE_NAME);

        customerProductDocMappings.addDocument(customer2.getContact(),
                                               product1,
                                               DeliveryNote.DELIVERY_NOTE_NAME);

        customerProductDocMappings.addDocument(customer2.getContact(),
                                               product2,
                                               CommercialInvoice.INVOICE_NAME);

        customerProductDocMappings.addDocument(customer2.getContact(),
                                               product2,
                                               DeliveryNote.DELIVERY_NOTE_NAME);

        customerProductDocMappings.addDocument(customer2.getContact(),
                                               product2,
                                               FDA_2887.FDA_2887_NAME);

        customerProductDocMappings.addDocument(customer2.getContact(),
                                               product2,
                                               FCC_740.FCC_740_NAME);

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

        List<ProductView> results = new ArrayList<>();
        for (Product product : products.values()) {
            results.add(new ProductView(product));
        }

        return results;
    }

    public List<DocumentView> getAllDocuments() {

        DocumentView commercialInvoiceView = getCommercialInvoice().getDocumentView();

        DocumentView deliveryNoteView = getDeliveryNote().getDocumentView();

        DocumentView fda2887View = getFDA2887().getDocumentView();

        return Arrays.asList(commercialInvoiceView,
                             deliveryNoteView,
                             fda2887View);
    }

    @Override
    public DocumentView getDocument(String documentName) {

        DocumentView result = null;

        switch (documentName) {
            case CommercialInvoice.INVOICE_NAME:
                CommercialInvoice commercialInvoice = getCommercialInvoice();
                result = commercialInvoice.getDocumentView();
                break;
            case DeliveryNote.DELIVERY_NOTE_NAME:
                DeliveryNote deliveryNote = getDeliveryNote();
                result = deliveryNote.getDocumentView();
                break;
            case FDA_2887.FDA_2887_NAME:
                FDA_2887 fda_2887 = getFDA2887();
                result = fda_2887.getDocumentView();
                break;
            case FCC_740.FCC_740_NAME:
                FCC_740 fcc_740 = getFCC740();
                result = fcc_740.getDocumentView();
                break;
            default:
                String message = String.format("No mapping present for documentName '%s'", documentName);
                throw new DocGenException(message);
        }

        return result;
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
    public Product getProduct(String productId) {
        if (!products.containsKey(productId)) {
            throw new DocGenException(String.format("product '%s' not found?!", productId));
        }
        return products.get(productId);
    }

    @Override
    public ContactView getVendor() {
        return vendor;
    }

    public abstract CommercialInvoice getCommercialInvoice();

    public abstract DeliveryNote getDeliveryNote();

    protected abstract FDA_2887 getFDA2887();

    protected abstract FCC_740 getFCC740();
}
