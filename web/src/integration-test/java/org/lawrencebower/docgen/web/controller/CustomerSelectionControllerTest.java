package org.lawrencebower.docgen.web.controller;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.web.model.SessionData;
import org.lawrencebower.docgen.web_logic.business.controler_business.customer_selection.CustomerSelectionCB;
import org.lawrencebower.docgen.doc_examples.ModelFactoryCodeImpl;
import org.lawrencebower.docgen.web_logic.view.contact.ContactView;
import org.lawrencebower.docgen.web_logic.view.product.Product;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.validation.support.BindingAwareModelMap;

import java.util.List;

import static junit.framework.Assert.assertTrue;
import static junit.framework.TestCase.assertEquals;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/web-application-test-context.xml")
public class CustomerSelectionControllerTest {

    @Autowired
    CustomerSelectionCB business;

    SessionData sessionData;

    CustomerSelectionController controller;

    @Before
    public void setUp() throws Exception {
        controller = new CustomerSelectionController();
        controller.setBusiness(business);
        sessionData = new SessionData();
        controller.setSessionData(sessionData);
    }

    @Test
    public void testShowHomePage_allCustomersPlacedOnModel() throws Exception {

        BindingAwareModelMap model = new BindingAwareModelMap();

        controller.showHomePage(model);

        List<ContactView> customers = (List<ContactView>) model.get("customers");

        assertEquals(2, customers.size());
    }

    @Test
    public void testSelectCustomer_validCustomer_customerSetOnSession() throws Exception {

        BindingAwareModelMap model = new BindingAwareModelMap();

        controller.selectCustomer(ModelFactoryCodeImpl.CUSTOMER_ID_1, model);

        ContactView customer = sessionData.getSelectedCustomer();
        assertEquals(ModelFactoryCodeImpl.CUSTOMER_ID_1, customer.getName());

    }

    @Test
    public void testSelectCustomer_validCustomer_businessSetOnSession() throws Exception {

        BindingAwareModelMap model = new BindingAwareModelMap();

        controller.selectCustomer(ModelFactoryCodeImpl.CUSTOMER_ID_1, model);

        ContactView business = sessionData.getSelectedBusiness();
        assertEquals(ModelFactoryCodeImpl.CUSTOMER_ID_2, business.getName());
    }

    @Test
    public void testSelectCustomer_validCustomer_productsSetOnModel() throws Exception {

        BindingAwareModelMap model = new BindingAwareModelMap();

        controller.selectCustomer(ModelFactoryCodeImpl.CUSTOMER_ID_1, model);

        List<Product> products = (List<Product>) model.get("products");

        boolean empty = products.isEmpty();
        assertTrue("No products set on page model", !empty);
    }
}
