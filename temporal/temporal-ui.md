
## The "Reset" button

Hint: The buttons in Temporal UI are mirrors of that is in `tctl` and the docs are usually there

The UI only allows me to reset to "Workflow Task Completed" points in history
    Is that just a limitation of the UI or more fundamental?
    Can I reset to any point using the CLI?

Q: does resetting throw away the old history?

The docs

> Reset a Workflow Execution so it can resume from a point in its Event History without losing its progress up to that point:
>
> ```
> temporal workflow reset \
>     --workflow-id YourWorkflowId \
>     --event-id YourLastEvent
> ```
>
> Start from where the Workflow Execution last continued as new:
>
> ```
> temporal workflow reset \
>     --workflow-id YourWorkflowId \
>     --type LastContinuedAsNew
> ```
>
> For batch resets, limit your resets to FirstWorkflowTask, LastWorkflowTask, or BuildId. Do not use Workflow IDs, run IDs, or event IDs with this command.