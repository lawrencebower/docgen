package org.lawrencebower.docgen.web_logic.business.controler_business.data_entry;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.web_model.business_def.utils.ViewableComponentFilter;
import org.lawrencebower.docgen.web_model.view.document.component.DocComponentView;
import org.lawrencebower.docgen.web_model.view.document.component.DocComponentViewFactory;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.util.Arrays;
import java.util.List;

import static junit.framework.TestCase.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.AutoMappedField.CUSTOMER_NAME;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/web-logic-test-context.xml")
public class ViewableComponentFilterTest {

    @Autowired
    DocComponentViewFactory viewFactory;

    private List<DocComponentView> components;

    @Before
    public void setup() {
        MockitoAnnotations.initMocks(this);
        components = getDocComponents();
    }

    @Autowired
    ViewableComponentFilter viewableComponentFilter;

    @Test
    public void testGetComponents_validParams_correctNumberReturned() throws Exception {
        List<DocComponentView> allComponents = getAllComponents();
        assertEquals(3, allComponents.size());
    }

    @Test
    public void testGetComponents_validParams_duplicateFieldsFiltered() throws Exception {
        List<DocComponentView> allComponents = getAllComponents();
        assertEquals("comp1", allComponents.get(0).getName());
        assertEquals(CUSTOMER_NAME.getName(), allComponents.get(1).getName());
        assertEquals("comp3", allComponents.get(2).getName());
    }

    @Test
    public void testGetComponents_validParams_automappedFieldsPresent() throws Exception {
        List<DocComponentView> allComponents = getAllComponents();
        assertTrue(allComponents.get(1).isAutoMapped());
    }

    private List<DocComponentView> getAllComponents() {
        return viewableComponentFilter.getComponents(components);
    }

    @Test
    public void testGetNonAutoMappedComponents_validParams_correctNumberReturned() throws Exception {
        List<DocComponentView> allComponents = getNonAutomappedComponents();
        assertEquals(2, allComponents.size());
    }

    private List<DocComponentView> getNonAutomappedComponents() {
        return viewableComponentFilter.getNonAutoMappedComponents(components);
    }

    @Test
    public void testGetNonAutoMappedComponents_validParams_duplicateFieldsFiltered() throws Exception {
        List<DocComponentView> allComponents = getAllComponents();
        assertEquals("comp1", allComponents.get(0).getName());
        assertEquals("comp3", allComponents.get(2).getName());
    }

    public List<DocComponentView> getDocComponents() {
        DocComponentView view1 = makeDocComponent("comp1");
        DocComponentView view2 = makeDocComponent(CUSTOMER_NAME.getName());
        DocComponentView view3 = makeDocComponent("comp3");
        DocComponentView view4 = makeDocComponent("comp1");//duplicate - should be filtered

        return Arrays.asList(view1,view2,view3,view4);
    }

    private DocComponentView makeDocComponent(String name) {
        TextComponent component1 = new TextComponent(name);
        component1.setName(name);
        return viewFactory.createTextComponentView(component1);
    }
}
