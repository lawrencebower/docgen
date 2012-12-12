package org.lawrencebower.docgen.web.controller.customer_selection;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.web_logic.business.controler_business.customer_selection.CustomerSelectionCB;
import org.lawrencebower.docgen.web_logic.view.contact.ContactView;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.validation.support.BindingAwareModelMap;

import java.util.List;

import static junit.framework.TestCase.assertEquals;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/web-application-test-context.xml")
public class ShowCustomersControllerTest {

    @Autowired
    CustomerSelectionCB business;

    ShowCustomersController controller;

    @Before
    public void setUp() throws Exception {
        controller = new ShowCustomersController();
        controller.setBusiness(business);
    }

    @Test
    public void testShowHomePage_allCustomersPlacedOnModel() throws Exception {

        BindingAwareModelMap model = new BindingAwareModelMap();

        controller.showHomePage(model);

        List<ContactView> customers = (List<ContactView>) model.get("customers");

        assertEquals(2, customers.size());
    }
}
