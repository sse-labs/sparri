package storage;

import com.rabbitmq.client.*;
import common.Configuration;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeoutException;

public class MessageQueuePublisher {

    private final Connection connection;
    private final Channel channel;

    private final Configuration config;

    private final String exchangeName = "lib-ident-exchange";
    private final String routingKey = "";

    public MessageQueuePublisher(Configuration config) throws IOException, TimeoutException {
        this.config = config;

        ConnectionFactory factory = new ConnectionFactory();
        factory.setUsername(config.getMqUsername());
        factory.setPassword(config.getMqPassword());
        factory.setVirtualHost("/");
        factory.setHost(config.getMqHost());
        factory.setPort(config.mqPort);

        connection = factory.newConnection("dependency-publisher-plugin");
        channel = connection.createChannel();
    }

    public void initialize() throws IOException {
        Map<String, Object> args = new HashMap<>();
        args.put("x-max-priority", 10);

        channel.exchangeDeclare(exchangeName, "direct", true);
        channel.queueDeclare(config.mqQueueName, true, false, false, args);
        channel.queueBind(config.mqQueueName, exchangeName, routingKey);
    }

    public void publishIdentifier(String ident) throws IOException {

        AMQP.BasicProperties props = new AMQP.BasicProperties.Builder()
                .contentType("text/plain")
                .deliveryMode(2)
                .priority(9)
                .build();

        channel.basicPublish(exchangeName, routingKey, props, ident.getBytes());
    }

    public void shutdown() throws IOException {
        connection.close();
    }

}
