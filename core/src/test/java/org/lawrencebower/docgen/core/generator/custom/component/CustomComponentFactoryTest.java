package org.lawrencebower.docgen.core.generator.custom.component;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.TableTextComponent;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import static org.junit.Assert.assertTrue;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/core-test-context.xml")
public class CustomComponentFactoryTest {

    @Autowired
    private CustomComponentFactory factory;

    @Test
    public void testCreateCustomComponent_unKnownType_throwsError(){
        try {
            DocComponent unknownComponent = new TableTextComponent("I am unsupported");
            factory.createCustomComponent(unknownComponent);
        } catch (DocGenException e) {
            assertTrue(e.getMessage().contains("DocComponent not mapped to CustomComponent?"));
        }
    }

}
