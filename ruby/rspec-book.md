

describe Foo
	context "when first created" do
		it "is empty" do
			f = Foo.new
			f.should be_empty
		end
	end
end

This makes the sentence
"Foo, when first created is empty"
"{thing-being-tested} when {in given context} {expected outcomes}"