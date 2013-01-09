package org.lawrencebower.docgen.web_model.view.contact;

import org.junit.Before;
import org.junit.Test;
import org.lawrencebower.docgen.core.exception.DocGenException;

import static junit.framework.TestCase.assertEquals;
import static org.mockito.Mockito.mock;

public class BusinessSelectionTest {

    private BusinessSelection businessSelection;

    @Before
    public void setUp() {
        businessSelection = new BusinessSelection();
    }

    @Test
    public void testSelectBusiness() throws Exception {
    //todo
    }

    @Test
    public void testGetSelectedBusiness() throws Exception {
        //todo
    }

    @Test
    public void testCheckBusinessSet_noBusiness_errorThrown() throws Exception {
        try {
            businessSelection.checkBusinessSet();
        } catch (DocGenException e) {
            String message = e.getMessage();
            assertEquals(BusinessSelection.NO_BUSINESS_SELECTED, message);
        }
    }

    @Test
    public void testCheckBusinessSet_businessSet_noErrorThrown() throws Exception {
        businessSelection.selectBusiness(mock(ContactView.class));
        businessSelection.checkBusinessSet();
    }
}
