run_analysis<-function(){

	#merging the training and test data sets
	train_data<-read.table("UCI HAR Dataset/train/X_train.txt")
	test_data<-read.table("UCI HAR Dataset/test/X_test.txt")
	merge_data<-rbind(train_data,test_data)
	
	#specifying column names for the recorded features
	features<-read.table("UCI HAR Dataset/features.txt")
	colnames(merge_data)<-features[,2]

	#merging the names of the subjects along with the activities for training data
	sub_training<-read.table("UCI HAR Dataset/train/subject_train.txt")
	act_training<-read.table("UCI HAR Dataset/train/y_train.txt")
	sub_act_training<-cbind(sub_training,act_training)
	colnames(sub_act_training)<-c("Subject","Activity")

	#merging the names of the subjects along with the activities for test data
	sub_test<-read.table("UCI HAR Dataset/test/subject_test.txt")
	act_test<-read.table("UCI HAR Dataset/test/y_test.txt")
	sub_act_test<-cbind(sub_test,act_test)
	colnames(sub_act_test)<-c("Subject","Activity")

	sub_act<-rbind(sub_act_training,sub_act_test)

	merge_data<-cbind(sub_act,merge_data)
	extract_data<-merge_data[,c(1,2,grep("mean",colnames(merge_data)),grep("std",colnames(merge_data)))]
	
	extract_data<-extract_data[order(extract_data[,1],extract_data[,2]),]
	
	temp<-split(extract_data,extract_data[,1])
	final_data<-vector()
	i<-1
	while(i<=30){
		temp2<-split(temp[[i]],temp[[i]][,2])
		temp3<-lapply(temp2,function(x)colMeans(x[,3:81]))
		Subject<-rep(i,6)
		temp3<-t(as.data.frame(temp3))
		Activities<-c(1:6)
		t4<-cbind(Subject,Activities,temp3)
		final_data<-rbind(final_data,t4)

		i<-i+1
	}
	#replacing activity no.s with names
	activities<-read.table("UCI HAR Dataset/activity_labels.txt")
	final_data[,2]<-replace(final_data[,2],final_data[,2]==1,activities[1,2])
	final_data[,2]<-replace(final_data[,2],final_data[,2]==2,activities[2,2])
	final_data[,2]<-replace(final_data[,2],final_data[,2]==3,activities[3,2])
	final_data[,2]<-replace(final_data[,2],final_data[,2]==4,activities[4,2])
	final_data[,2]<-replace(final_data[,2],final_data[,2]==5,activities[5,2])
	final_data[,2]<-replace(final_data[,2],final_data[,2]==6,activities[6,2])

	write.table(final_data, "Final_Data.txt", sep=",",row.names=FALSE)
}
