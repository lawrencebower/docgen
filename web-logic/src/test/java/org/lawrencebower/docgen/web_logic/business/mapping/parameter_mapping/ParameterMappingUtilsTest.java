package org.lawrencebower.docgen.web_logic.business.mapping.parameter_mapping;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_model.view.constants.ViewConstants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import static junit.framework.TestCase.assertEquals;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/web-logic-test-context.xml")
public class ParameterMappingUtilsTest {

    @Autowired
    ParameterMappingUtils parameterMappingUtils;

    @Test(expected = DocGenException.class)
    public void testSplitStringBySeparator_nullArg_throwsError() {
        parameterMappingUtils.splitStringBySeparator(null);
    }

    public void testSplitStringBySeparator_singleArg_tokenLength1() {
        String[] tokens = parameterMappingUtils.splitStringBySeparator("single token");
        assertEquals(1, tokens.length);
    }

    public void testSplitStringBySeparator_multiArg_multiTokenLength1() {

        StringBuilder builder = new StringBuilder();
        builder.append("token 1");
        builder.append(ViewConstants.DOCUMENT_FIELD_SEPARATOR);
        builder.append("token 2");
        builder.append(ViewConstants.DOCUMENT_FIELD_SEPARATOR);
        builder.append("token 3");

        String string = builder.toString();

        String[] tokens = parameterMappingUtils.splitStringBySeparator(string);
        assertEquals(3, tokens.length);
    }


}
