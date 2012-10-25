<%@ include file="/WEB-INF/jsp/include.jsp" %>

hello I am the home page

<jsp:useBean id="customers" scope="request"
             type="java.util.List<org.lawrencebower.docgen.web_model.view.business.ContactView>"/>

<div>
    <ol>
        <c:forEach var="product" items="${customers}">

            <s:url var="customer_url" value="/customerSelect/customerName/{customerName}">
                <s:param name="customerName" value="${product.contactName}"/>
            </s:url>

            <a href="${customer_url}">
                <c:out value="${product.contactName}"/>
            </a>
            &nbsp;<c:out value="${product.contactAddress}"/><br/>
        </c:forEach>
    </ol>
</div>
