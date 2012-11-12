<%@ include file="/WEB-INF/jsp/include.jsp" %>

hello I am the home page

<jsp:useBean id="customers" scope="request"
             type="java.util.List<org.lawrencebower.docgen.web_model.view.contact.ContactView>"/>

<div>
    <ol>
        <c:forEach var="product" items="${customers}">

            <s:url var="customer_url" value="/customerSelect/customerName/{customerName}">
                <s:param name="customerName" value="${product.name}"/>
            </s:url>

            <a href="${customer_url}">
                <c:out value="${product.name}"/>
            </a>
            &nbsp;<c:out value="${product.address}"/><br/>
        </c:forEach>
    </ol>
</div>
