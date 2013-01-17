package org.lawrencebower.docgen.web.controller.product_selection;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.web.controller.data_entry.SessionSetupUtils;
import org.lawrencebower.docgen.web.model.SessionData;
import org.lawrencebower.docgen.web_logic.business.controler_business.product_selection.ProductSelectionCB;
import org.lawrencebower.docgen.web_model.view.view_factory.ViewFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/web-application-test-context.xml")
@Ignore
public class ProductDetailsControllerTest {

    @Autowired
    ProductSelectionCB productSelectionBusiness;
    @Autowired
    ViewFactory viewFactory;
    @Autowired
    SessionSetupUtils sessionSetupUtils;
    @Autowired
    private SessionData sessionData;

    private ProductDetailsController controller;

    @Before
    public void setup() {
        controller = new ProductDetailsController();
        controller.setBusiness(productSelectionBusiness);
        controller.setSessionData(sessionData);
        setupSessionData();
    }

    private void setupSessionData() {
        sessionSetupUtils.setupSessionData(sessionData);
    }

}
