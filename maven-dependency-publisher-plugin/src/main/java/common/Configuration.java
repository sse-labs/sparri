package common;

public class Configuration {

    public final String mqQueueName = "library-identifiers";
    public final int mqPort = 8080;

    private String mqHost = "ls5vs029.cs.tu-dortmund.de";
    private String mqUsername = "my-user";
    private String mqPassword = "<CHANGEME>";

    public Configuration(String paramMqUser, String paramMqPassword, String paramMqHost){
        if(!paramMqUser.isEmpty()){
            this.mqUsername = paramMqUser;
        }

        if(!paramMqPassword.isEmpty()){
            this.mqPassword = paramMqPassword;
        }

        if(!paramMqHost.isEmpty()){
            this.mqHost = paramMqHost;
        }
    }

    public String getMqHost(){ return this.mqHost; }

    public String getMqUsername() { return this.mqUsername; }

    public String getMqPassword() { return this.mqPassword; }
}
