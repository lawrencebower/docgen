package org.lawrencebower.docgen.web_model.view.view_factory.tsv_factory;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.lawrencebower.docgen.web_model.view.view_factory.factory.CustomerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.util.Map;

import static junit.framework.TestCase.assertEquals;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/web-model-integration-test-context.xml")
public class CustomerFactoryTsvImplTest {

    @Autowired
    private CustomerFactory customerFactory;

    @Test
    public void testGetCustomers_validFile_correctNumberReturned() throws Exception {
        Map<String, ContactView> customers = customerFactory.getCustomers();
        assertEquals(2, customers.size());
    }
}
