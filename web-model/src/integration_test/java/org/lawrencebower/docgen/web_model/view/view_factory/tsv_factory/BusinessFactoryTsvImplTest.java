package org.lawrencebower.docgen.web_model.view.view_factory.tsv_factory;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.web_model.view.view_factory.factory.BusinessFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.util.Map;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/web-model-integration-test-context.xml")
public class BusinessFactoryTsvImplTest {

    @Autowired
    private BusinessFactory businessFactory;

    @Test
    public void testGetBusinesses() throws Exception {
        Map<String,String> businesses = businessFactory.getBusinesses();
        int i = 0;
    }
}
