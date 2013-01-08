package org.lawrencebower.docgen.web_logic.business.utils;

import org.junit.Ignore;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.web_model.business_def.utils.ViewUtils;
import org.lawrencebower.docgen.web_model.view.document.DocumentViewFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/web-logic-test-context.xml")
@Ignore
public class ViewUtilsTest {

    @Autowired
    ViewUtils viewUtils;
    @Autowired
    DocumentViewFactory viewFactory;

}
