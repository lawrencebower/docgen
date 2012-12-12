package org.lawrencebower.docgen.web.controller.data_entry;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.web.model.SessionData;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import static junit.framework.Assert.assertFalse;
import static junit.framework.Assert.assertTrue;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/web-application-test-context.xml")
public class ToggleAutomappedControllerTest {

    SessionData sessionData;
    ToggleAutomappedController controller;

    @Before
    public void setUp() throws Exception {

        sessionData = new SessionData();

        controller = new ToggleAutomappedController();
        controller.setSessionData(sessionData);
    }

    @Test
    public void testToggleShowAutomappedFields_showAutoMappedTrue_valueToggled() throws Exception {
        sessionData.setShowAutoMappedFields(true);
        controller.toggleShowAutomappedFields();
        assertFalse(sessionData.isShowAutoMappedFields());
    }

    @Test
    public void testToggleShowAutomappedFields_showAutoMappedFalse_valueToggled() throws Exception {
        sessionData.setShowAutoMappedFields(false);
        controller.toggleShowAutomappedFields();
        assertTrue(sessionData.isShowAutoMappedFields());
    }
}
